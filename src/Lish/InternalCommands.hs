{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lish internal commands
module Lish.InternalCommands
  ( lookup
  , toArg
  )
  where

import           Data.Fix
import qualified Data.Map.Strict    as Map
import qualified Data.Text          as Text
import           GHC.IO.Handle      (hGetContents)
import           Protolude          hiding (show)
import           System.Environment (setEnv)

import           Lish.Parser        (parseCmd)
import           Lish.Types

toArg :: SExp -> StateT Env IO (Maybe Text)
toArg (Atom x)          = do
  env <- get
  return $ Just $ case Map.lookup x env of
    Just (Str s) -> s
    _            -> toS x
toArg (Str s)           = return $ Just $ toS s
toArg (Num i) = return . Just . toS . show $ i
toArg (Stream (Just h)) = lift $ fmap (Just . Text.strip .toS) (hGetContents h)
toArg (List xs) = do
  strs <- traverse toArg (map unFix xs)
  return (Just ("["<> (Text.intercalate " " (catMaybes strs)) <> "]"))
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

-- | Undefine a var
undef :: ReduceUnawareCommand
undef ((Atom name):[]) = do
  modify (Map.delete name)
  return Void
undef x = evalErr $ "undef wait an atom got" <> toS (show x)

-- | retrieve the value of a var
getenv :: ReduceUnawareCommand
getenv ((Atom varname):[]) = do
  hm <- get
  return $ fromMaybe Void (Map.lookup varname hm)
getenv ((Str varname):[]) = do
  hm <- get
  return $ fromMaybe Void (Map.lookup varname hm)
getenv _ = evalErr "getenv need on atom or a string as argument"

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
binop _ exprs = evalErr
  ("binary operator needs two numbers. Got: " <> toS (show exprs))

bbinop :: (Bool -> Bool -> Bool) -> ReduceUnawareCommand
bbinop f ((Bool x):(Bool y):[]) = return $ Bool (f x y)
bbinop _ _ = evalErr "boolean binary operator need two booleans arguments"

lnot :: ReduceUnawareCommand
lnot ((Bool x):[]) = return ( Bool (not x))
lnot _             = evalErr "not need a boolean"

toWaitingStream :: ReduceUnawareCommand
toWaitingStream (Stream (Just h) :[]) = return (WaitingStream (Just h))
toWaitingStream _                     = return Void

bintest :: (Integer -> Integer -> Bool) -> ReduceUnawareCommand
bintest f ((Num x):(Num y):[]) = return $ Bool (f x y)
bintest _ args = evalErr $ "bin test need two numbers got " <> (toS (show args))

isReduced :: SExp -> Bool
isReduced (Atom _)   = False
isReduced (Lambda _) = False
isReduced _          = True

deepReduce :: (Monad m) => (SExp -> m SExp) -> SExp -> m SExp
deepReduce f x =
  if isReduced x
  then pure x
  else do
    reducedOnce <- f x
    deepReduce f reducedOnce

toStrictCmd :: ReduceUnawareCommand -> Command
toStrictCmd f reducer sexps =
  f =<< mapM (deepReduce reducer) sexps

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
                        , body = Fix . Lambda . map Fix $ (Atom "do"):bodies
                        , closure = mempty
                        , types = ([],LCommand)
                        })
        else return Void
    _ -> return Void
  where fromAtom (Fix (Atom a)) = Just a
        fromAtom _              = Nothing
fn _ _ = return Void

strictCommands :: [(Text,ReduceUnawareCommand)]
strictCommands = [ ("prn", prn)
                 , ("pr", pr)
                 , (">", toWaitingStream)
                 , ("replace", replace)
                 , ("undef",undef)
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
                 -- bin numeric test
                 , ("<",bintest (<))
                 , ("<=",bintest (<=))
                 , (">",bintest (>))
                 , (">=",bintest (>=))
                 -- boolean bin ops
                 , ("and", bbinop (&&))
                 , ("or", bbinop (||))
                 , ("not", lnot)
                 ]

-- | Define a var
def :: Command
def _ ((Atom name):v:[]) = do
  modify (Map.insert name v)
  return v
def _ exprs =
  evalErr $ "def need 2 args, an atom and an S-Expr. Ex: (def foo \"foo\")"
            <> "instead got: " <> toS (show exprs)

doCommand :: Command
doCommand reduceLambda exprs = do
  foldl' (\_ x -> reduceLambda x) (return Void) exprs

lishIf :: Command
lishIf reduceLambda (sexp:sexp1:sexp2:[]) = do
  reducedSexp <- reduceLambda sexp
  case reducedSexp of
    Bool True  -> reduceLambda sexp1
    Bool False -> reduceLambda sexp2
    _          -> evalErr "first argument to if must be a Bool"
lishIf _ _ = evalErr "if need a bool, a then body and an else one"

emptyCmd :: Command
emptyCmd _ ((List []):[]) = return (Bool True)
emptyCmd _ ((List _):[]) = return (Bool False)
emptyCmd r (x@(Atom _):[]) = do
  val <- r x
  emptyCmd r (val:[])
emptyCmd r (x@(Lambda _):[]) = do
  val <- r x
  emptyCmd r (val:[])
emptyCmd _ _ = return Void

firstCmd :: Command
firstCmd reducer ((List (x:_)):[]) = reducer (unFix x)
firstCmd _ ((List _):[]) = return Void
firstCmd r (x@(Atom _):[]) = do
  val <- r x
  firstCmd r (val:[])
firstCmd r (x@(Lambda _):[]) = do
  val <- r x
  firstCmd r (val:[])
firstCmd _ _ = return Void

restCmd :: Command
restCmd _ ((List (_:xs)):[]) = return (List xs)
restCmd _ ((List _):[]) = return Void
restCmd r (x@(Atom _):[]) = do
  val <- r x
  restCmd r (val:[])
restCmd r (x@(Lambda _):[]) = do
  val <- r x
  restCmd r (val:[])
restCmd _ _ = return Void

consCmd :: Command
consCmd r (x:(List ls):[]) = do
  xreduced <- r x
  return (List (Fix xreduced:ls))
consCmd r (x:y@(Atom _):[]) = do
  val <- r y
  consCmd r (x:val:[])
consCmd r (x:y@(Lambda _):[]) = do
  val <- r y
  consCmd r (x:val:[])
consCmd _ _ = return Void

equal :: Command
equal r ((List xs):(List ys):[]) = do
  reducedListX <- traverse r (map unFix xs)
  reducedListY <- traverse r (map unFix ys)
  return (Bool (reducedListX == reducedListY))
equal r (x:y:[]) = do
  reducedX <- r x
  reducedY <- r y
  return (Bool (reducedX == reducedY))
equal _ args     = evalErr $ "= need two args, got " <> (toS (show args))

-- | Export a var as Environment variable
export :: Command
export _ ((Atom name):v@(Str s):[]) = do
  liftIO $ setEnv (toS name) (toS s)
  modify (Map.insert name v)
  return v
export r (n:value:[]) = do
  reducedVal <- r value
  export r (n:reducedVal:[])
export _ _ = evalErr $ "eval need an atom and a string (eval foo \"foo\")"

-- ## TODO
-- eval :: Command
-- eval r ((Str program):[]) = do
--   let parsed = parseCmd program
--   case parsed of
--     Right expr -> r (unFix expr)
--     _          -> evalErr "eval error"
-- eval r (x@(Atom _):[]) = do
--   reduced <- r x
--   eval r (reduced:[])
-- eval r (x@(Lambda _):[]) = do
--   reduced <- r x
--   eval r (reduced:[])
-- eval _ _ = evalErr "eval error"

unstrictCommands :: [(Text,InternalCommand)]
unstrictCommands = [ ("if", InternalCommand "if" lishIf)
                   , ("def", InternalCommand "def" def)
                   , ("fn", InternalCommand "fn" fn)
                   , ("do", InternalCommand "do" doCommand)
                   , ("=", InternalCommand "=" equal)
                   , ("export", InternalCommand "export" export)
                   -- , ("eval", InternalCommand "eval" eval)
                   -- list ops
                   , ("empty?",InternalCommand "empty?" emptyCmd)
                   , ("first",InternalCommand "first" firstCmd)
                   , ("rest",InternalCommand "rest" restCmd)
                   , ("cons",InternalCommand "cons" consCmd)
                   ]

internalCommands :: Map.Map Text InternalCommand
internalCommands = (strictCommands & map (\(x,y) -> (x,InternalCommand x (toStrictCmd y))))
                   <> unstrictCommands
  & Map.fromList

lookup :: Text -> Maybe InternalCommand
lookup = flip Map.lookup internalCommands
