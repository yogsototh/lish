{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish core
module Lish.Core
  (
    runLish
  ) where

import qualified Control.Exception        as Exception
import           Data.Maybe               (catMaybes, isJust)
import           GHC.IO.Handle            (Handle, hGetContents)
import           Pipes
import           Prelude                  (String, lines, lookup)
import           Protolude                hiding (for, many, show, (<|>))
import           System.Console.Haskeline
import           System.Process
import           Text.Parsec              (ParseError)

import           Lish.InternalCommands
import           Lish.Parser
import           Lish.Types

-- | Start an interactive lish shell
runLish :: IO ()
runLish = runInputT defaultSettings mainLoop

mainLoop :: InputT IO ()
mainLoop = do
  maybeLine <- getInputLine ":€ > "
  case maybeLine of
    -- EOF / control-d
    Nothing -> outputStrLn "bye bye!"
    Just "exit" -> outputStrLn "bye bye!"
    Just "logout" -> outputStrLn "bye bye!"
    Just line -> do
      eval (parseCmd ("(" <> line <> ")"))
      mainLoop

toStdIn :: SExp -> Maybe Handle
toStdIn (WaitingStream h) = h
toStdIn _                 = Nothing

shellErr :: String -> IO SExp
shellErr errmsg = do
  putStrLn ("Error: " <> errmsg)
  return Void

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

-- | Evaluate a command line
eval :: Either ParseError SExp -> InputT IO ()
eval parsed = case parsed of
  Right sexp -> liftIO (reduceLambda sexp >>= evalReduced)
  Left err   -> outputStrLn (show err)

evalReduced :: SExp -> IO ()
evalReduced Void = return ()
evalReduced (Stream Nothing) = return ()
evalReduced (Stream (Just h)) = do
  cmdoutput <- hGetContents h
  let splittedLines = lines cmdoutput
      producer = mapM_ yield splittedLines
  runEffect (for producer (lift . putStrLn))
evalReduced (WaitingStream Nothing) = return ()
evalReduced (WaitingStream (Just h)) = do
  cmdoutput <- hGetContents h
  let splittedLines = lines cmdoutput
      producer = mapM_ yield splittedLines
  runEffect (for producer (lift . putStrLn))
evalReduced x = putStrLn (show x)

reduceLambda :: SExp -> IO SExp
reduceLambda (Lambda exprs) = do
  reduced <- mapM reduceLambda exprs
  case reduced of
    (Atom f:args) -> case lookup f internalCommands of
                        Just fn -> fn args
                        _       -> executeShell (Lambda reduced)
    _             ->  executeShell (Lambda reduced)
reduceLambda x          = return x
