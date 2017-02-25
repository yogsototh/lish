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
reduceLambda (Lambda exprs) = do
  reduced <- mapM reduceLambda exprs
  case reduced of
    (Atom f:args) -> do
      resultInternal <- tryInternalCommand f args
      case resultInternal of
        Just x -> return x
        Nothing -> do
          resultEnv <- tryEnvCommand f args
          case resultEnv of
            Just x  -> return x
            Nothing -> lift (executeShell (Lambda reduced))
    _             ->  lift (executeShell (Lambda reduced))
reduceLambda x          = return x

apply :: SExp -> Command
apply = undefined

tryEnvCommand :: Text -> [SExp] -> StateT Env IO (Maybe SExp)
tryEnvCommand f args = do
  envcmd <- get
  case Map.lookup f envcmd of
    Just fn -> Just <$> (apply fn args)
    _       -> return Nothing


tryInternalCommand :: Text -> [SExp] -> StateT Env IO (Maybe SExp)
tryInternalCommand f args =
  case InternalCommands.lookup f of
    Just fn -> Just <$> fn args
    _       -> return Nothing

-- | take a SExp
toStdIn :: SExp -> Maybe Handle
toStdIn (WaitingStream h) = h
toStdIn _                 = Nothing

shellErr :: Text -> IO SExp
shellErr errmsg = do
  putText ("Error: " <> errmsg)
  return Void

-- | Execute a shell command
executeShell :: SExp -> IO SExp
executeShell (Lambda args) = do
  res <- (mapM toArg args) >>= return . catMaybes
  let argsHandle = (filter isJust (map toStdIn args))
      stdinhandle = case argsHandle of
                      (Just h:_) -> UseHandle h
                      _          -> Inherit
  case (map toS res) of
    (cmd:sargs) -> do
      result <- trySh $ createProcess (proc cmd sargs) { std_in = stdinhandle
                                                       , std_out = CreatePipe }
      case result of
        Right (_, mb_hout, _, _) -> return $ Stream mb_hout
        Left ex                  -> shellErr ("[shell 1/2] " <> (show (Lambda args)) <> "\n[shell 2/2] " <> show ex)
    _ -> shellErr "empty lambda!"
  where
    trySh :: IO a -> IO (Either IOException a)
    trySh = Exception.try
executeShell _ = shellErr "[shell] not a lambda!"
