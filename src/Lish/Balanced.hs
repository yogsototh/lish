{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Check if a Lish expression is correctly balanced
module Lish.Balanced
  ( checkBalanced
  , Stack
  , Balanced(..)
  )
  where

import Protolude
import qualified Data.Text as T
import Data.Stack (Stack, pop, push)

data Balanced = Balanced | Unbalanced Char deriving (Eq, Show)

checkBalanced :: Text -> Stack Char -> Balanced
checkBalanced (T.uncons -> Just ('(',suf)) stk = checkBalanced suf (push stk '(')
checkBalanced (T.uncons -> Just ('[',suf)) stk = checkBalanced suf (push stk '[')
checkBalanced (T.uncons -> Just ('{',suf)) stk = checkBalanced suf (push stk '{')

checkBalanced (T.uncons -> Just (')',suf)) (pop -> Just ('(',stk)) = checkBalanced suf stk
checkBalanced (T.uncons -> Just (')',_)) _ = Unbalanced ')'
checkBalanced (T.uncons -> Just (']',suf)) (pop -> Just ('[',stk)) = checkBalanced suf stk
checkBalanced (T.uncons -> Just (']',_)) _ = Unbalanced ']'
checkBalanced (T.uncons -> Just ('}',suf)) (pop -> Just ('{',stk)) = checkBalanced suf stk
checkBalanced (T.uncons -> Just ('}',_)) _ = Unbalanced '}'

checkBalanced (T.uncons -> Just (_,suf)) stk = checkBalanced suf stk
checkBalanced _ (pop -> Just (x,_)) = Unbalanced x
checkBalanced _ _ = Balanced
