{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish core
module Lish.Core
  (
    runLish
  ) where

import           GHC.IO.Handle            (hGetContents)
import           Pipes
import           Prelude                  (lines)
import           Protolude                hiding (for, many, show, (<|>))
import           System.Console.Haskeline
import           Text.Parsec              (ParseError)

import           Lish.Eval
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

eval :: Either ParseError SExp -> InputT IO ()
eval parsed = case parsed of
  Right sexpr -> liftIO (reduceLambda (EnvSExp sexpr []) >>= evalReduced .sexp)
  Left err   -> outputStrLn (show err)
