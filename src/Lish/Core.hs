{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish core
module Lish.Core
  (
    runLish
  ) where

import qualified Data.Map.Strict          as Map
import           GHC.IO.Handle            (hGetContents)
import           Pipes
import           Prelude                  (String, lines)
import           Protolude                hiding (for, many, show, (<|>))
import           System.Console.Haskeline
import           System.Environment       (getEnvironment)
import           Text.Parsec              (ParseError)

import           Lish.Eval
import           Lish.Parser
import           Lish.Types

-- | Start an interactive lish shell
runLish :: IO ()
runLish = do
  env <- toEnv <$> getEnvironment
  runInputT defaultSettings (mainLoop env)

-- | System Environment -> LISH Env
toEnv :: [(String,String)] -> Env
toEnv env =
  env &
  map (\(k,v) -> (toS k, Str (toS v))) &
  Map.fromList

-- | Main REPL loop / Interpreter
mainLoop :: Env -> InputT IO ()
mainLoop env = do
  let prompt = case Map.lookup "PROMPT" env of
                 Just (Str p) -> p
                 _            -> ":€ > "
  maybeLine <- getInputLine (toS prompt)
  case maybeLine of
    -- EOF / control-d
    Nothing -> outputStrLn "bye bye!"
    Just "exit" -> outputStrLn "bye bye!"
    Just "logout" -> outputStrLn "bye bye!"
    Just line -> do
      newenv <- eval env (parseCmd ("(" <> line <> ")"))
      mainLoop newenv

-- | Eval the reduced form
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

-- | Evaluate the parsed expr
eval :: Env -> Either ParseError SExp -> InputT IO Env
eval env parsed = case parsed of
  Right sexpr -> liftIO $ do
    (reduced,newenv) <- runStateT (reduceLambda sexpr) env
    evalReduced reduced
    return newenv
  Left err   -> outputStrLn (show err) >> return env
