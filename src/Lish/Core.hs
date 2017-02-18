{-# LANGUAGE OverloadedStrings #-}
-- | Lish core
module Lish.Core
  (
    runLish
  ) where

import           Control.Monad.IO.Class
import           Data.List                (intercalate)
import           GHC.IO.Handle            (hGetContents)
import           System.Console.Haskeline
import           System.Process
import           Text.Parsec

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
      eval (parseCmd line)
      mainLoop

data Cmd = Cmd String [String] deriving (Eq, Show)

type Command = [String] -> IO ()

internalCommands :: [(String,Command)]
internalCommands = [("pr",\args -> putStrLn (intercalate " " args))]

-- PARSE
parseCmd :: String -> Either ParseError Cmd
parseCmd = parse sExprParser "S-Expr"

identifier :: Parsec String () String
identifier = many1 (noneOf " \t")

sExprParser :: Parsec String () Cmd
sExprParser = do
  (cmdname:args) <- many1 (identifier <* spaces)
  return $ Cmd cmdname args

-- EVAL
internalFunction :: String -> Maybe Command
internalFunction cmdname = lookup cmdname internalCommands

execute :: String -> [String] -> IO ()
execute cmd args = do
    res <- createProcess (proc cmd args) { std_out = CreatePipe }
    case res of
      (_, Just hout, _, _) -> hGetContents hout >>= mapM_ putStrLn . (map ("o-o " ++)) . lines
      _                    -> putStrLn "no output"

eval :: Either ParseError Cmd -> InputT IO ()
eval parsed = case parsed of
  Right (Cmd cmdname args) -> case internalFunction cmdname of
                                Just f -> liftIO (f args)
                                Nothing -> liftIO (execute cmdname args)
  Left err -> outputStrLn (show err)
