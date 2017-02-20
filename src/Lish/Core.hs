{-# LANGUAGE OverloadedStrings #-}
-- | Lish core
module Lish.Core
  (
    runLish
  ) where

import           Control.Monad.IO.Class
import           Data.Char                (isSpace)
import           Data.Maybe               (catMaybes, isJust)
import           GHC.IO.Handle            (Handle, hGetContents)
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
      eval (parseCmd $ "(" ++ line ++ ")")
      mainLoop

data SExp = S [SExp]
          | Atom String
          | Str String
          | Stream CmdStream
          | WaitingStream CmdStream

-- | a Command is a function that takes arguments
-- and then returns an output that will be a list of lines
-- type CmdStream = Producer String IO ()
type CmdStream = Maybe Handle

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

-- prn :: Command
-- prn str _ = do
--   lift $ putStrLn (intercalate " " str)
--   return ()
--
-- pr :: Command
-- pr str _ = do
--   lift $ putStr (intercalate " " str)
--   return ()
--
--
-- internalCommands :: [(String,Command)]
-- internalCommands = [("prn",prn)
--                    , ("pr", pr)]
--
-- internalFunction :: String -> Maybe Command
-- internalFunction cmdname = lookup cmdname internalCommands

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

toArg :: SExp -> IO (Maybe String)
toArg (Atom x)          = return $ Just x
toArg (Str s)           = return $ Just s
toArg (Stream (Just h)) = fmap (Just . trim) (hGetContents h)
toArg _                 = return $ Nothing

toStdIn :: SExp -> Maybe Handle
toStdIn (WaitingStream h) = h
toStdIn _                 = Nothing

executeShell :: SExp -> IO SExp
executeShell (S args) = do
  res <- (mapM toArg args) >>= return . catMaybes
  let argsHandle = (filter isJust (map toStdIn args))
  case res of
    (cmd:sargs) -> do
      (_, mb_hout, _, _) <- createProcess (proc cmd sargs) { std_in = case argsHandle of
                                                                        (Just h:_) -> UseHandle h
                                                                        _          -> Inherit
                                                           , std_out = CreatePipe }
      return $ Stream mb_hout
    _ -> error "Empty list to execute!"
executeShell _ = error "Can't execute something that is not an atom!"

-- | Evaluate a command line
eval :: Either ParseError SExp -> InputT IO ()
eval parsed = case parsed of
  Right sexp -> liftIO (reduce sexp >>= evalReduced)
  Left err   -> outputStrLn (show err)

evalReduced :: SExp -> IO ()
evalReduced (Atom s) = putStrLn s
evalReduced (Str s) = print s
evalReduced (S _) = putStrLn "Unreduced SExp!!!!"
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

reduce :: SExp -> IO SExp
reduce (S exprs) = do
  reduced <- mapM reduce exprs
  executeShell (S reduced)
reduce x          = return x
