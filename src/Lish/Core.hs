{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish core
module Lish.Core
  (
    runLish
  ) where

import qualified Control.Exception        as Exception
import           Data.Char                (isSpace)
import           Data.List                (intercalate)
import           Data.Maybe               (catMaybes, isJust)
import qualified Data.Text                as Text
import           GHC.IO.Handle            (Handle, hGetContents)
import           Pipes
import           Prelude                  (String, lines, lookup)
import           Protolude                hiding (for, many, (<|>))
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
      eval (parseCmd ("(" <> line <> ")"))
      mainLoop

data SExp = Lambda [SExp]
          | Void
          | Atom Text
          | Str Text
          | List [SExp]
          -- Temporal values only present during reduction/evaluation
          | Stream CmdStream
          | WaitingStream CmdStream

-- | a Command is a function that takes arguments
-- and then returns an output that will be a list of lines
-- type CmdStream = Producer String IO ()
type CmdStream = Maybe Handle

-- | = PARSE

parseCmd :: String -> Either ParseError SExp
parseCmd = parse parseExpr "S-Expr"

parseExpr :: Parsec String () SExp
parseExpr = parseLambda
            <|> parseList
            <|> parseAtom
            <|> parseString

parseAtom :: Parsec String () SExp
parseAtom = Atom <$> do frst <- (noneOf " \t()[]\"")
                        rest <- many (noneOf " \t()[]")
                        return $ toS (frst:rest)

parseString :: Parsec String () SExp
parseString = (Str . toS) <$> between (char '"')
                              (char '"')
                              (many (noneOf "\""))

parseSExps :: Parsec String () [SExp]
parseSExps = sepBy parseExpr spaces

parseLambda :: Parsec String () SExp
parseLambda = Lambda <$> between (char '(') (char ')') parseSExps

parseList :: Parsec String () SExp
parseList = List <$> between (char '[') (char ']') parseSExps



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

evalErr :: Text -> IO SExp
evalErr errmsg = do
  putText $ "EvalError: " <> errmsg
  return Void

replace :: Command
replace ((Str old):(Str new):(Str str):[]) =
  return $ Str $ Text.replace old new str
replace _ = evalErr "replace should take 3 String arguments"

toWaitingStream :: Command
toWaitingStream (Stream (Just h):[]) = return (WaitingStream (Just h))
toWaitingStream _                    = return Void

type Command = [SExp] -> IO SExp

internalCommands :: [(Text,Command)]
internalCommands = [ ("prn", prn)
                   , ("pr", pr)
                   , (">", toWaitingStream)
                   , ("replace", replace)
                   ]
--
-- internalFunction :: String -> Maybe Command
-- internalFunction cmdname = lookup cmdname internalCommands

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

toArg :: SExp -> IO (Maybe String)
toArg (Atom x)          = return $ Just $ toS x
toArg (Str s)           = return $ Just $ toS s
toArg (Stream (Just h)) = fmap (Just . trim) (hGetContents h)
toArg _                 = return $ Nothing

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
  case res of
    (cmd:sargs) -> do
      result <- trySh $ createProcess (proc cmd sargs) { std_in = stdinhandle
                                                       , std_out = CreatePipe }
      case result of
        Right (_, mb_hout, _, _) -> return $ Stream mb_hout
        Left ex                  -> shellErr ("[shell 1/2] " <> repr (Lambda args) <> "\n[shell 2/2] " <> show ex)
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

repr :: SExp -> String
repr (Atom s)        = toS s
repr (Str s)         = "\"" <> toS s <> "\""
repr (Lambda sexprs) = "(λ." <> (intercalate " " (map repr sexprs)) <> ")"
repr (List sexprs)   = "[" <> (intercalate " " (map repr sexprs)) <> "]"
repr _               = "<?>"

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
evalReduced x = putStrLn (repr x)

reduceLambda :: SExp -> IO SExp
reduceLambda (Lambda exprs) = do
  reduced <- mapM reduceLambda exprs
  case reduced of
    (Atom f:args) -> case lookup f internalCommands of
                        Just fn -> fn args
                        _       -> executeShell (Lambda reduced)
    _             ->  executeShell (Lambda reduced)
reduceLambda x          = return x
