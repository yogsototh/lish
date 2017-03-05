
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish parser
module Lish.Eval
  ( reduceLambda
  , checkType
  )
  where

import qualified Control.Exception     as Exception
import qualified Data.Map.Strict       as Map
import           Protolude
import           System.Process        hiding (env)

import           Lish.InternalCommands (toArg)
import qualified Lish.InternalCommands as InternalCommands
import           Lish.Types            hiding (show)

-- | Infer the type of an expression
infer :: Context -> SExp -> Either TypeError LishType
infer _ Void = return LVoid
infer _ (Num _) = return LNum
infer _ (Bool _) = return LBool
infer _ (Str _) = return LStr
infer ctx (List (expr:exprs)) = do
  case infer ctx expr of
    Left terr -> Left terr
    Right t -> case mapM (\e -> checkType ctx e t) exprs of
      Left terror -> Left terror
      Right _     -> return $ LList t
infer ctx (Atom a) = case Map.lookup a ctx of
  Just t  -> return t
  Nothing -> Left . TypeError $ "Undefined atom: " <> toS a
infer ctx (Fn parameters fnbody _ (ptypes,retType)) = do
  let newCtx = Map.union ctx (Map.fromList (zip parameters ptypes))
  checkType newCtx fnbody retType
  return $ LFn ptypes retType
infer ctx (Lambda ((Fn fnparams _ _ (ptypes,retType)):exprs)) =
  if length fnparams /= length exprs
  then Left (TypeError "Fn applied to the wrong number of parameters")
  else do
    inferedTypes <- mapM (infer ctx) exprs
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

-- | The main evaluation function
-- its real type should be something isomorphic to
-- (SExp,Environment) -> IO (SExp, Environment)
reduceLambda :: SExp -> StateT Env IO SExp
reduceLambda (Lambda (expr:exprs)) = do
  reduced <- reduceLambda expr
  redred <- reduceLambda reduced
  if redred /= reduced
    then reduceLambda (Lambda (reduced:exprs))
    else do
      -- DEBUG --env <- get
      -- DEBUG --liftIO $ do
      -- DEBUG --  putText "Lambda:"
      -- DEBUG --  print $ (expr:exprs)
      -- DEBUG --  putText "Env:"
      -- DEBUG --  print env
      -- DEBUG --  putText "Reduced Head:"
      -- DEBUG --  print reduced
      case reduced of
        Atom f -> do
          resultInternal <- tryInternalCommand f exprs
          case resultInternal of
            Just x -> return x
            Nothing -> do
              resultEnv <- tryEnvCommand f exprs
              case resultEnv of
                Just x  -> return x
                Nothing -> do
                  reducedArgs <- mapM reduceLambda exprs
                  executeShell (Lambda ((Atom f):reducedArgs))
        f@(Fn _ _ _ _) -> applyFn f exprs
        s  -> do
          reducedArgs <- mapM reduceLambda exprs
          executeShell (Lambda (s:reducedArgs))
reduceLambda (Atom x) = do
  env <- get
  case Map.lookup x env of
    Just s -> return s
    _      -> return $ Atom x
reduceLambda x          = return x

applyFn :: SExp -> ReduceUnawareCommand
applyFn (Fn par bod clos _) args =
  if length par /= length args
    then shellErr "wrong number of arguments"
    else do
      let localClosure = bindVars clos (zip par args)
      currentEnv <- get
      -- Run the function in its own closure
      fmap fst $ liftIO $
        runStateT (reduceLambda bod) (Map.union currentEnv localClosure)
  where
    bindVars oldenv newvars = Map.union oldenv (Map.fromList newvars)
applyFn x _ = return x

tryEnvCommand :: Text -> [SExp] -> StateT Env IO (Maybe SExp)
tryEnvCommand f args = do
  envcmd <- get
  case Map.lookup f envcmd of
    Just fn@(Fn _ _ _ _) -> Just <$> (applyFn fn args)
    _                    -> return Nothing


tryInternalCommand :: Text -> [SExp] -> StateT Env IO (Maybe SExp)
tryInternalCommand f args =
  case InternalCommands.lookup f of
    Just (fn) -> Just <$> fn reduceLambda args
    _         -> return Nothing

-- | take a SExp
toStdIn :: SExp -> Maybe Handle
toStdIn (WaitingStream h) = h
toStdIn _                 = Nothing

shellErr :: Text -> StateT Env IO SExp
shellErr errmsg = do
  putText ("Error: " <> errmsg)
  return Void

-- | Execute a shell command
executeShell :: SExp -> StateT Env IO SExp
executeShell (Lambda args) = do
  res <- (mapM toArg args) >>= return . catMaybes
  let argsHandle = (filter isJust (map toStdIn args))
      stdinhandle = case argsHandle of
                      (Just h:_) -> UseHandle h
                      _          -> Inherit
  case (map toS res) of
    (cmd:sargs) -> do
      result <- lift $ trySh $ createProcess (proc cmd sargs) { std_in = stdinhandle
                                                              , std_out = CreatePipe }
      case result of
        Right (_, mb_hout, _, _) -> return $ Stream mb_hout
        Left ex                  -> shellErr ("Unknow fn or cmd: "
                                              <> toS cmd
                                              <> "\n" <> show ex)
    _ -> shellErr "empty lambda!"
  where
    trySh :: IO a -> IO (Either IOException a)
    trySh = Exception.try
executeShell _ = shellErr "[shell] not a lambda!"
