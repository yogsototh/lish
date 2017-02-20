{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish parser
module Lish.Eval
  ( reduceLambda
  )
  where

import qualified Control.Exception        as Exception
import           Protolude
import           System.Process hiding (env)
import           Prelude                  (lookup)

import           Lish.Types hiding (show)
import Lish.InternalCommands

-- | The main evaluation function
-- TODO: its real type should be something isomorphic to
-- (SExp,Environment) -> IO (SExp, Environment)
reduceLambda :: EnvSExp -> IO EnvSExp
reduceLambda (EnvSExp { sexp = (Lambda exprs)
                      , env = environment
                      }) = do
  reduced <- mapM reduceLambda (map (\sexpr -> EnvSExp sexpr environment) exprs)
  case reduced of
    (EnvSExp { sexp = Atom f, env = cmdenv}:args)
      -> case lookup f internalCommands of
           Just fn -> fn args
           _       -> executeShell (Lambda (map sexp reduced)) >>= \s -> return $ EnvSExp { sexp = s, env = cmdenv }
    _ ->  executeShell (Lambda (map sexp reduced)) >>= \s -> return $ EnvSExp { sexp = s, env = environment }
reduceLambda x          = return x

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
