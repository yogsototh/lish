
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish parser
module Lish.Eval
  ( reduceLambda
  )
  where

import qualified Control.Exception     as Exception
import qualified Data.Map.Strict       as Map
import           Protolude
import           System.Process        hiding (env)

import           Lish.InternalCommands (toArg)
import qualified Lish.InternalCommands as InternalCommands
import           Lish.Types            hiding (show)

-- | The main evaluation function
-- TODO: its real type should be something isomorphic to
-- (SExp,Environment) -> IO (SExp, Environment)
reduceLambda :: SExp -> StateT Env IO SExp
reduceLambda (Lambda (expr:exprs)) = do
  reduced <- reduceLambda expr
  env <- get
  liftIO $ print env
  liftIO $ print reduced
  liftIO $ print exprs
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
    f@(Fn _ _ _) -> applyFn f exprs
    s  -> do
      reducedArgs <- mapM reduceLambda exprs
      executeShell (Lambda (s:reducedArgs))
reduceLambda (Atom x) = do
  env <- get
  case Map.lookup x env of
    Just s -> return s
    _ -> return $ Atom x
reduceLambda x          = return x

applyFn :: SExp -> ReduceUnawareCommand
applyFn (Fn par bod clos) args =
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
    Just fn@(Fn _ _ _) -> Just <$> (applyFn fn args)
    _       -> return Nothing


tryInternalCommand :: Text -> [SExp] -> StateT Env IO (Maybe SExp)
tryInternalCommand f args =
  case InternalCommands.lookup f of
    Just (fn) -> Just <$> fn reduceLambda args
    _       -> return Nothing

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
        Left ex                  -> shellErr ("[shell 1/2] " <> (show (Lambda args)) <> "\n[shell 2/2] " <> show ex)
    _ -> shellErr "empty lambda!"
  where
    trySh :: IO a -> IO (Either IOException a)
    trySh = Exception.try
executeShell _ = shellErr "[shell] not a lambda!"
