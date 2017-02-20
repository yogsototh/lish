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
  strs <- catMaybes <$> (mapM toArg args)
  putText (Text.intercalate " " strs)
  return Void

pr :: Command
pr args = do
  strs <- catMaybes <$> (mapM toArg args)
  putText (Text.intercalate " " strs)
  return Void

evalErr :: Text -> IO SExp
evalErr errmsg = do
  putText $ "EvalError: " <> errmsg
  return Void

replace :: Command
replace ((Str old):(Str new):(Str str):[]) =
  return . Str $ Text.replace old new str
replace _ = evalErr "replace should take 3 String arguments"

toWaitingStream :: Command
toWaitingStream ((Stream (Just h)):[]) = return (WaitingStream (Just h))
toWaitingStream _                      = return Void

internalCommands :: [(Text,Command)]
internalCommands = [ ("prn", prn)
                   , ("pr", pr)
                   , (">", toWaitingStream)
                   , ("replace", replace)
                   ]
