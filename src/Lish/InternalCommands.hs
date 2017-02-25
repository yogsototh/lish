{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish internal commands
module Lish.InternalCommands
  ( lookup
  , toArg
  )
  where

import qualified Data.Map.Strict    as Map
import qualified Data.Text          as Text
import           GHC.IO.Handle      (hGetContents)
import           Lish.Types
import           Protolude          hiding (show)
import           System.Environment (setEnv)

toArg :: SExp -> IO (Maybe Text)
toArg (Atom x)          = return $ Just $ toS x
toArg (Str s)           = return $ Just $ toS s
toArg (Stream (Just h)) = fmap (Just . Text.strip .toS) (hGetContents h)
toArg _                 = return $ Nothing

prn :: Command
prn args = do
  strs <- catMaybes <$> liftIO (mapM toArg args)
  putStrLn $ (Text.intercalate " " strs)
  return Void

pr :: Command
pr args = do
  strs <- catMaybes <$> liftIO (mapM toArg args)
  putStr (Text.intercalate " " strs)
  return Void

evalErr :: Text -> StateT Env IO SExp
evalErr errmsg = do
  putText $ "EvalError: " <> errmsg
  return Void

llet :: Command
llet ((Atom name):v:[]) = do
  modify (Map.insert name v)
  return v
llet _ = return Void

export :: Command
export ((Atom name):v@(Str s):[]) = do
  liftIO $ setEnv (toS name) (toS s)
  modify (Map.insert name v)
  return v
export _ = return Void

getenv :: Command
getenv ((Atom varname):[]) = do
  hm <- get
  return $ fromMaybe Void (Map.lookup varname hm)
getenv _ = return Void

replace :: Command
replace ((Str old) : (Str new) : (Str text) : []) =
  return $ Str $ Text.replace old new text
replace _ = evalErr "replace should take 3 String arguments"

str :: Command
str exprs = do
  args <- catMaybes <$> liftIO (mapM toArg exprs)
  return $ Str $ Text.concat args

atom :: Command
atom ((Atom a):[]) = return $ Atom a
atom ((Str s):[])  = return $ Atom s
atom _             = return Void

toWaitingStream :: Command
toWaitingStream (Stream (Just h) :[]) = return (WaitingStream (Just h))
toWaitingStream _                     = return Void

internalCommands :: Map.Map Text Command
internalCommands = [ ("prn", prn)
                   , ("pr", pr)
                   , (">", toWaitingStream)
                   , ("replace", replace)
                   , ("let",llet)
                   , ("export",export)
                   , ("getenv",getenv)
                   , ("$",getenv)
                   , ("str",str)
                   , ("atom",atom)
                   ] & Map.fromList

lookup :: Text -> Maybe Command
lookup = flip Map.lookup internalCommands
