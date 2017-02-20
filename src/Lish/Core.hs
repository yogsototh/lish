{-# LANGUAGE OverloadedStrings #-}
-- | Lish core
module Lish.Core
  (
    runLish
  ) where

import           Control.Monad.IO.Class
import           Data.Char                (isSpace)
import           Data.List                (intercalate)
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

data SExp = Lambda [SExp]
          | Void
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
parseList = fmap Lambda $ sepBy parseExpr spaces

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
prn strs = do
  args <- fmap catMaybes (mapM toArg strs)
  putStrLn (intercalate " " args)
  return Void

pr :: Command
pr strs = do
  args <- fmap catMaybes (mapM toArg strs)
  putStr (intercalate " " args)
  return Void

toWaitingStream :: Command
toWaitingStream (Stream (Just h):[]) = return (WaitingStream (Just h))
toWaitingStream _ = return Void

type Command = [SExp] -> IO SExp

internalCommands :: [(String,Command)]
internalCommands = [ ("prn", prn)
                   , ("pr", pr)
                   , (">", toWaitingStream)
                   ]
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
executeShell (Lambda args) = do
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
evalReduced Void = return ()
evalReduced (Atom s) = putStrLn s
evalReduced (Str s) = print s
evalReduced (Lambda _) = putStrLn "Unreduced SExp!!!!"
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
reduce (Lambda exprs) = do
  reduced <- mapM reduce exprs
  case reduced of
    (Atom f:args) -> case lookup f internalCommands of
                        Just fn -> fn args
                        _ -> executeShell (Lambda reduced)
    _             ->  executeShell (Lambda reduced)
reduce x          = return x
