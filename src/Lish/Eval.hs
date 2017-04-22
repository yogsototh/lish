
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish parser
module Lish.Eval
  ( checkType
  , reduceLambda
  )
  where

import qualified Control.Exception     as Exception
import           Data.Fix
import qualified Data.Map.Strict       as Map
import           Prelude               (String)
import           Protolude
import           System.Process        hiding (env)
import qualified System.Process        as Process
import qualified Text.Show.Pretty      as Pr

import           Lish.InternalCommands (toArg)
import qualified Lish.InternalCommands as InternalCommands
import           Lish.Types            hiding (show)

-- | Infer the type of an expression
infer :: Context -> SExp -> Either TypeError LishType
infer _ Void = return LVoid
infer _ (Num _) = return LNum
infer _ (Bool _) = return LBool
infer _ (Str _) = return LStr
infer ctx (List ((Fix expr):exprs)) = do
  case infer ctx expr of
    Left terr -> Left terr
    Right t -> case mapM (\e -> checkType ctx e t) (map unFix exprs) of
      Left terror -> Left terror
      Right _     -> return $ LList t
infer ctx (Atom a) = case Map.lookup a ctx of
  Just t  -> return t
  Nothing -> Left . TypeError $ "Undefined atom: " <> toS a
infer ctx (Fn parameters fnbody _ (ptypes,retType)) = do
  let newCtx = Map.union ctx (Map.fromList (zip parameters ptypes))
  checkType newCtx (unFix fnbody) retType
  return $ LFn ptypes retType
infer ctx (Lambda ((Fix (Fn fnparams _ _ (ptypes,retType))):exprs)) =
  if length fnparams /= length exprs
  then Left (TypeError "Fn applied to the wrong number of parameters")
  else do
    inferedTypes <- mapM (infer ctx) (map unFix exprs)
    if inferedTypes /= ptypes
      then Left . TypeError $ "Expected " <> show ptypes
                              <> " bug got " <> show inferedTypes
      else return retType
infer _ (Lambda _) = Left . TypeError $ "First element of a lambda must be a Fn or a Command"
infer _ sexp = Left . TypeError $ "can't infer the type of " <> show sexp

-- | Check the type of some expression regarding a type context
checkType :: Context -> SExp -> LishType -> Either TypeError ()
checkType ctx expr ty = infer ctx expr >>= \ inferedType ->
  if inferedType == ty
  then return ()
  else Left (TypeError ("Expected Type" <> show ty
                         <> " but got type " <> show inferedType))

isReduced :: SExp -> Bool
isReduced (Atom _)   = False
isReduced (Lambda _) = False
isReduced _          = True

-- | The main evaluation function
-- its real type should be something isomorphic to
-- (SExp,Environment) -> IO (SExp, Environment)
_reduceLambda :: SExp -> StateT Env IO SExp
_reduceLambda (Lambda (Fix expr:fexprs)) = do
  let exprs = map unFix fexprs
  reduced <- reduceLambda expr
  if isReduced reduced
    then do
      case reduced of
        Internal command -> (_commandFn command) reduceLambda exprs
        f@(Fn _ _ _ _) -> applyFn f exprs
        s  -> do
          reducedArgs <- mapM reduceLambda exprs
          executeCommand (Cmd (Fix s) (map Fix reducedArgs))
    else reduceLambda (Lambda . map Fix $ (reduced:exprs))
_reduceLambda command@(Internal  _) = executeCommand command
_reduceLambda (Atom x) = do
  env <- get
  case Map.lookup x env of
    Just s -> return s
    _      -> case InternalCommands.lookup x of
      Just cmd -> return (Internal cmd)
      _        -> return (Str x)
_reduceLambda x          = return x

reduceLambda :: SExp -> StateT Env IO SExp
reduceLambda x = do
  env <- get
  case (Map.lookup "LISH_DEBUG" env) of
    Just (Str "true") -> liftIO $ do
      putText "------"
      putStr ("Env: " :: Text)
      putStrLn $ Pr.ppShow env
      putStr ("Arg: " :: Text)
      putStrLn $ pprint (Fix x)
    _ -> return ()
  _reduceLambda x


applyFn :: SExp -> ReduceUnawareCommand
applyFn (Fn par bod clos _) args =
  if length par /= length args
    then shellErr "wrong number of arguments"
    else do
      reducedArgs <- mapM reduceLambda args
      let localClosure = bindVars clos (zip par reducedArgs)
      currentEnv <- get
      -- Run the function in its own closure
      fmap fst $ liftIO $
        runStateT (reduceLambda (unFix bod)) (Map.union localClosure currentEnv)
  where
    bindVars oldenv newvars = Map.union oldenv (Map.fromList newvars)
applyFn x _ = return x

-- | take a SExp
toStdIn :: SExp -> Maybe Handle
toStdIn (WaitingStream h) = h
toStdIn _                 = Nothing

shellErr :: Text -> StateT Env IO SExp
shellErr errmsg = do
  putText ("Error: " <> errmsg)
  return Void

-- | Execute a shell command
executeCommand :: SExp -> StateT Env IO SExp
executeCommand (Cmd (Fix (Str cmdName)) args) = do
  lispEnv <- get
  res <- (mapM toArg (map unFix args)) >>= return . catMaybes
  let argsHandle = (filter isJust (map toStdIn (map unFix args)))
      stdinhandle = case argsHandle of
                      (Just h:_) -> UseHandle h
                      _          -> Inherit
      localCmdEnv = mkLocalCmdEnv lispEnv
  case (map toS res) of
    sargs -> do
      result <- lift . trySh $
        createProcess (proc (toS cmdName) sargs)
                      { std_in = stdinhandle
                      , std_out = CreatePipe
                      , Process.env = localCmdEnv
                      }
      case result of
        Right (_, mb_hout, _, _) -> return $ Stream mb_hout
        Left ex                  -> shellErr ("Unknow fn or cmd: "
                                              <> toS cmdName
                                              <> "\n" <> show ex)
  where
    trySh :: IO a -> IO (Either IOException a)
    trySh = Exception.try
    mkLocalCmdEnv :: Map.Map Text SExp -> Maybe [(String,String)]
    mkLocalCmdEnv m = traverse pairToProcessPair (Map.toList m)
    pairToProcessPair :: (Text,SExp) -> Maybe (String,String)
    pairToProcessPair (x,Str y) = Just (toS x,toS y)
    pairToProcessPair (_,_)     = Nothing
executeCommand _ = shellErr "[shell] not a lambda!"
