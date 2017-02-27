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
prn :: ReduceUnawareCommand
prn args = do
  strs <- catMaybes <$> mapM toArg args
  putStrLn $ (Text.intercalate " " strs)
  return Void

-- | Print
pr :: ReduceUnawareCommand
pr args = do
  strs <- catMaybes <$> mapM toArg args
  putStr (Text.intercalate " " strs)
  return Void

evalErr :: Text -> StateT Env IO SExp
evalErr errmsg = do
  putText $ "EvalError: " <> errmsg
  return Void

-- | Define a var
def :: ReduceUnawareCommand
def ((Atom name):v:[]) = do
  modify (Map.insert name v)
  return v
def _ = evalErr "def need 2 args, an atom and an S-Expr. Ex: (def foo \"foo\")"

-- | Undefine a var
undef :: ReduceUnawareCommand
undef ((Atom name):[]) = do
  modify (Map.delete name)
  return Void
undef x = evalErr $ "undef wait an atom got" <> toS (show x)

-- | Export a var as Environment variable
export :: ReduceUnawareCommand
export ((Atom name):v@(Str s):[]) = do
  liftIO $ setEnv (toS name) (toS s)
  modify (Map.insert name v)
  return v
export _ = evalErr $ "eval need an atom and a string (eval foo \"foo\")"

-- | retrieve the value of a var
getenv :: ReduceUnawareCommand
getenv ((Atom varname):[]) = do
  hm <- get
  return $ fromMaybe Void (Map.lookup varname hm)
getenv _ = evalErr "getenv need on atom as argument"

-- | replace à la `sed s/old/new/g text`
replace :: ReduceUnawareCommand
replace ((Str old) : (Str new) : (Str text) : []) =
  return $ Str $ Text.replace old new text
replace _ = evalErr "replace should take 3 String arguments"

-- | create a string and concat multiple elements
str :: ReduceUnawareCommand
str exprs = do
  args <- catMaybes <$> mapM toArg exprs
  return $ Str $ Text.concat args

-- | create an atom from a string (do nothing to atoms)
atom :: ReduceUnawareCommand
atom ((Atom a):[]) = return $ Atom a
atom ((Str s):[])  = return $ Atom s
atom _             = evalErr "atom need an atom or a string"

-- | Numbers Ops
binop :: (Integer -> Integer -> Integer) -> ReduceUnawareCommand
binop f ((Num x):(Num y):[]) = return $ Num (f x y)
binop _ _ = evalErr "binary operator needs two numbers"

bbinop :: (Bool -> Bool -> Bool) -> ReduceUnawareCommand
bbinop f ((Bool x):(Bool y):[]) = return $ Bool (f x y)
bbinop _ _ = evalErr "boolean binary operator need two booleans arguments"

lnot :: ReduceUnawareCommand
lnot ((Bool x):[]) = return ( Bool (not x))
lnot _ = evalErr "not need a boolean"

toWaitingStream :: ReduceUnawareCommand
toWaitingStream (Stream (Just h) :[]) = return (WaitingStream (Just h))
toWaitingStream _                     = return Void

toStrictCmd :: ReduceUnawareCommand -> Command
toStrictCmd f reducer sexps = do
  reduced <- mapM reducer sexps
  f reduced

-- | fn to declare a lish function
-- (fn [arg1 arg2] body1 body2)
-- (fn [x y] (if (> x 3) (* 2 x) (* y y)))
fn :: Command
fn reducer (p:bodies) = do
  reducedParams <- reducer p
  case reducedParams of
    List args -> do
      let  parameters = map fromAtom args
      if all isJust parameters
        then return (Fn { params = catMaybes parameters
                        , body = bodies
                        , closure = mempty
                        })
        else return Void
    _ -> return Void
  where fromAtom (Atom a) = Just a
        fromAtom _ = Nothing
fn _ _ = return Void

strictCommands :: [(Text,ReduceUnawareCommand)]
strictCommands = [ ("prn", prn)
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
                 -- binary operators
                 , ("+",binop (+))
                 , ("-",binop (-))
                 , ("*",binop (*))
                 , ("/",binop div)
                 , ("^",binop (^))
                 -- boolean bin ops
                 , ("and", bbinop (&&))
                 , ("or", bbinop (||))
                 , ("not", lnot)
                 ]

lishIf :: Command
lishIf reduceLambda (sexp:sexp1:sexp2:[]) = do
  reducedSexp <- reduceLambda sexp
  case reducedSexp of
    Bool True -> reduceLambda sexp1
    Bool False -> reduceLambda sexp2
    _ -> evalErr "first argument to if must be a Bool"
lishIf _ _ = evalErr "if need a bool, a then body and an else one"

unstrictCommands :: [(Text,Command)]
unstrictCommands = [ ("if",lishIf)
                   , ("fn",fn)
                   ]

internalCommands :: Map.Map Text Command
internalCommands = (strictCommands & map (\(x,y) -> (x,toStrictCmd y)))
                   <> unstrictCommands
  & Map.fromList

lookup :: Text -> Maybe Command
lookup = flip Map.lookup internalCommands
