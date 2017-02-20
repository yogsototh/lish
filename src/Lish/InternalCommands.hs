{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish internal commands
module Lish.InternalCommands
  ( internalCommands
  , toArg
  )
  where

import           GHC.IO.Handle            (hGetContents)
import qualified Data.Text  as Text
import           Lish.Types
import           Protolude

toArg :: SExp -> IO (Maybe Text)
toArg (Atom x)      = return $ Just $ toS x
toArg (Str s)       = return $ Just $ toS s
toArg (Stream (Just h)) = fmap (Just . Text.strip .toS) (hGetContents h)
toArg _                 = return $ Nothing

prn :: Command
prn args = do
  strs <- catMaybes <$> (mapM (toArg . sexp) args)
  putText $ (Text.intercalate " " strs) <> "\n"
  return EnvSExp { sexp = Void
                 , env = (mconcat (map env args))
                 }

pr :: Command
pr args = do
  strs <- catMaybes <$> (mapM (toArg . sexp) args)
  putText (Text.intercalate " " strs)
  return EnvSExp { sexp = Void
                 , env = (mconcat (map env args))
                 }

evalErr :: Text -> IO EnvSExp
evalErr errmsg = do
  putText $ "EvalError: " <> errmsg
  return (EnvSExp Void empty)

replace :: Command
replace args@((EnvSExp { sexp = (Str old)}) :
              (EnvSExp { sexp = (Str new)}) :
              (EnvSExp { sexp = (Str str)}) :
              []) =
  return $ EnvSExp { sexp =  Str $ Text.replace old new str
                   , env = (mconcat (map env args))}
replace _ = evalErr "replace should take 3 String arguments"

toWaitingStream :: Command
toWaitingStream (EnvSExp { sexp = Stream (Just h)
                         , env = environment     }:[]) =
  return $ EnvSExp (WaitingStream (Just h)) environment
toWaitingStream _                      = return (EnvSExp Void empty)

internalCommands :: [(Text,Command)]
internalCommands = [ ("prn", prn)
                   , ("pr", pr)
                   , (">", toWaitingStream)
                   , ("replace", replace)
                   ]
