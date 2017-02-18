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

data SExp = S [SExp]
          | Str String
          deriving (Eq, Show)

-- | a Command is a function that takes arguments
-- and then returns an output that will be a list of lines
type Command = [String] -> IO [String]

-- | = PARSE

parseCmd :: String -> Either ParseError SExp
parseCmd = parse parseExpr "S-Expr"

identifier :: Parsec String () String
identifier = many1 (noneOf " \t")

parseList :: Parsec String () SExp
parseList = fmap S $ sepBy parseExpr spaces

parseExpr :: Parsec String () SExp
parseExpr = do
  S (cmdname:args) <- between (char '(')
                              (char ')')
                              parseList
  return $ Cmd cmdname args

-- |
-- = EVAL

-- |
-- == INTERNAL COMMANDS

prn :: Command
prn str = do
  putStrLn (intercalate " " str)
  return []

pr :: Command
pr str = do
  putStr (intercalate " " str)
  return []


internalCommands :: [(String,Command)]
internalCommands = [("prn",prn)
                   , ("pr", pr)]

internalFunction :: String -> Maybe Command
internalFunction cmdname = lookup cmdname internalCommands

execute :: SExp -> IO [String]
execute (Cmd cmd args) = do
    res <- createProcess (proc cmd args) { std_out = CreatePipe }
    case res of
      (_, Just hout, _, _) -> hGetContents hout >>= return . lines
      _                    -> putStrLn "no output" >> return []
execute _ = error "execute not on Cmd! This should never have happened!"

-- | Evaluate a command line
eval :: Either ParseError SExp -> InputT IO ()
eval parsed = case parsed of
  Right sexp -> liftIO (seval sexp) >>= mapM_ outputStr
  Left err -> outputStrLn (show err)

seval :: SExp -> IO [String]
seval (cmd@(Cmd cmdname args)) = case internalFunction cmdname of
                                   Just f -> f args
                                   Nothing -> execute cmd
seval (S exprs) = do
  lsOflines <- mapM seval exprs
  case map unlines lsOflines of
    (cmd:args) -> seval (Cmd cmd args)
    _ -> putStrLn "ERROR in seval, empty S-Expr" >> return []

seval (Str s) = return [s]
