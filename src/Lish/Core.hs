{-# LANGUAGE OverloadedStrings #-}
-- | Lish core
module Lish.Core
  (
    runLish
  , internalCommands
  , internalFunction
  ) where

import           Control.Monad.IO.Class
import           Data.List                (intercalate)
import           GHC.IO.Handle            (hGetContents)
import           Pipes
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
          | Atom String
          | Str String
          | Stream CmdStream

-- | a Command is a function that takes arguments
-- and then returns an output that will be a list of lines
type CmdStream = Producer String IO ()
type Command = [String] -> CmdStream -> CmdStream

-- | = PARSE

parseCmd :: String -> Either ParseError SExp
parseCmd = parse parseExpr "S-Expr"

identifier :: Parsec String () String
identifier = many1 (noneOf " \t()")

parseList :: Parsec String () SExp
parseList = fmap S $ sepBy parseExpr spaces

parseExpr :: Parsec String () SExp
parseExpr = between (char '(')
                    (char ')')
                    parseList
            <|> fmap Atom identifier

-- |
-- = EVAL

-- |
-- == INTERNAL COMMANDS

prn :: Command
prn str _ = do
  lift $ putStrLn (intercalate " " str)
  return ()

pr :: Command
pr str _ = do
  lift $ putStr (intercalate " " str)
  return ()


internalCommands :: [(String,Command)]
internalCommands = [("prn",prn)
                   , ("pr", pr)]

internalFunction :: String -> Maybe Command
internalFunction cmdname = lookup cmdname internalCommands

-- | Extremely unsafe
unatom :: SExp -> String
unatom (Atom x) = x
unatom _ = error "BAD, Expected atom and got something else"

execute :: SExp -> IO SExp
execute (S (Atom cmd:args)) = do
    res <- createProcess (proc cmd (map unatom args)) { std_out = CreatePipe }
    case res of
      (_, Just hout, _, _) -> do
        cmdoutput <- hGetContents hout
        let splitedByLines = lines cmdoutput
            producer = mapM_ yield splitedByLines
        return $ Stream producer
      _                    -> do
        putStrLn "no output"
        return $ Stream (return ())
execute _ = error "execute not on Cmd! This should never have happened!"

-- | Evaluate a command line
eval :: Either ParseError SExp -> InputT IO ()
eval parsed = case parsed of
  Right sexp -> liftIO (reduce sexp >>= evalReduced)
  Left err   -> outputStrLn (show err)

evalReduced :: SExp -> IO ()
evalReduced (Atom s) = putStrLn s
evalReduced (Str s) = print s
evalReduced (S _) = putStrLn "Unreduced SExp!!!!"
evalReduced (Stream cmdStream) =
  runEffect (for cmdStream (lift . putStrLn))

reduce :: SExp -> IO SExp
reduce sexp@(S _) = execute sexp
reduce x = return x
