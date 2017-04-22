{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish core
module Lish.Core
  (
    runLish
  ) where

import           Data.Fix
import qualified Data.Map.Strict          as Map
import           GHC.IO.Handle            (hGetContents)
import           Pipes
import           Prelude                  (String, lines)
import           Protolude                hiding (for, many, show, (<|>))
import           System.Console.Haskeline
import           System.Environment       (getEnvironment)
import           Text.Parsec              (ParseError)

import           Lish.Balanced            (Balanced (..), checkBalanced)
import           Lish.Eval                (reduceLambda)
import           Lish.Parser              (parseCmd)
import           Lish.Types

-- | Start an interactive lish shell
runLish :: IO ()
runLish = do
  env <- toEnv <$> getEnvironment
  -- load core
  runInputT (defaultSettings { historyFile = Just ".lish-history" })
    (do
        -- load lish core
        fileContent <- liftIO $ readFile "lish/core.lsh"
        newEnv <- eval env (fmap unFix (parseCmd ("(do " <> fileContent <> ")")))
        mainLoop Nothing newEnv "")

-- | System Environment -> LISH Env
toEnv :: [(String,String)] -> Env
toEnv env =
  env &
  map (\(k,v) -> (toS k, Str (toS v))) &
  Map.fromList

-- | Main REPL loop / Interpreter
-- the first argument is a @Maybe Char@ it contains the char in the stack
-- that verify if the expression is balanced.
-- So if the first argument is not Nothing, it means we are in the middle
-- of a multiline expression.
mainLoop :: Maybe Char -- ^ Check to know if we are in the middle of the writting of a multiline expression
         -> Env -- ^ The Lish environement
         -> Text -- ^ The previous partial input (if in the middle of a multiline expression)
         -> InputT IO ()
mainLoop mc env previousPartialnput = do
  maybeLine <- getInputLine (toS (prompt mc env))
  case maybeLine of
    x | x `elem` [ Nothing -- EOF / control-d
                 , Just "bye"
                 , Just "exit"
                 , Just "logout"] -> outputStrLn "bye bye!"

    Just line -> do
      let exprs = previousPartialnput
                    <> (if isJust mc then " " else "")
                    <> toS line
      case checkBalanced exprs empty of
        Unbalanced c -> mainLoop (Just c) env exprs
        Balanced -> do
          newenv <- eval env (fmap unFix (parseCmd ("(" <> exprs <> ")")))
          mainLoop Nothing newenv ""

    _ -> panic "That should NEVER Happens, please file bug"

prompt :: Maybe Char -> Env -> Text
prompt mc env = case mc of
                  Just _ -> ">>> "
                  Nothing -> case Map.lookup "PROMPT" env of
                     Just (Str p) -> p
                     _            -> ":€ > "

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
evalReduced x = putStrLn (pprint (Fix x))

-- | Evaluate the parsed expr
eval :: Env -> Either ParseError SExp -> InputT IO Env
eval env parsed = case parsed of
  Right sexpr -> liftIO $ do
    (reduced,newenv) <- runStateT (reduceLambda sexpr) env
    evalReduced reduced
    return newenv
  Left err   -> outputStrLn (show err) >> return env
