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

toArg :: SExp -> StateT Env IO (Maybe Text)
toArg (Atom x)          = do
  env <- get
  return $ Just $ case Map.lookup x env of
    Just (Str s) -> s
    _ -> toS x
toArg (Str s)           = return $ Just $ toS s
toArg (Stream (Just h)) = lift $ fmap (Just . Text.strip .toS) (hGetContents h)
toArg _                 = return $ Nothing

-- | Print with return line
prn :: Command
prn args = do
  strs <- catMaybes <$> mapM toArg args
  putStrLn $ (Text.intercalate " " strs)
  return Void

-- | Print
pr :: Command
pr args = do
  strs <- catMaybes <$> mapM toArg args
  putStr (Text.intercalate " " strs)
  return Void

evalErr :: Text -> StateT Env IO SExp
evalErr errmsg = do
  putText $ "EvalError: " <> errmsg
  return Void

-- | Define a var
def :: Command
def ((Atom name):v:[]) = do
  modify (Map.insert name v)
  return v
def _ = evalErr "def need 2 args, an atom and an S-Expr. Ex: (def foo \"foo\")"

-- | Undefine a var
undef :: Command
undef ((Atom name):[]) = do
  modify (Map.delete name)
  return Void
undef x = evalErr $ "undef wait an atom got" <> toS (show x)

-- | Export a var as Environment variable
export :: Command
export ((Atom name):v@(Str s):[]) = do
  liftIO $ setEnv (toS name) (toS s)
  modify (Map.insert name v)
  return v
export _ = evalErr $ "eval need an atom and a string (eval foo \"foo\")"

-- | retrieve the value of a var
getenv :: Command
getenv ((Atom varname):[]) = do
  hm <- get
  return $ fromMaybe Void (Map.lookup varname hm)
getenv _ = evalErr "getenv need on atom as argument"

-- | replace à la `sed s/old/new/g text`
replace :: Command
replace ((Str old) : (Str new) : (Str text) : []) =
  return $ Str $ Text.replace old new text
replace _ = evalErr "replace should take 3 String arguments"

-- | create a string and concat multiple elements
str :: Command
str exprs = do
  args <- catMaybes <$> mapM toArg exprs
  return $ Str $ Text.concat args

-- | create an atom from a string (do nothing to atoms)
atom :: Command
atom ((Atom a):[]) = return $ Atom a
atom ((Str s):[])  = return $ Atom s
atom _             = evalErr "atom need an atom or a string"

toWaitingStream :: Command
toWaitingStream (Stream (Just h) :[]) = return (WaitingStream (Just h))
toWaitingStream _                     = return Void

internalCommands :: Map.Map Text Command
internalCommands = [ ("prn", prn)
                   , ("pr", pr)
                   , (">", toWaitingStream)
                   , ("replace", replace)
                   , ("def",def)
                   , ("undef",undef)
                   , ("export",export)
                   , ("getenv",getenv)
                   , ("$",getenv)
                   , ("str",str)
                   , ("atom",atom)
                   ] & Map.fromList

lookup :: Text -> Maybe Command
lookup = flip Map.lookup internalCommands
