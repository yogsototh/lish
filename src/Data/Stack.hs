{-# LANGUAGE NoImplicitPrelude #-}
module Data.Stack
  ( Stack
  , pop
  , top
  , push
  , size
  )
  where

import Protolude

-- | Stack data structure
data Stack a = Stack [a] deriving (Eq,Show)

instance Functor Stack where
  fmap f (Stack xs) = Stack (fmap f xs)

instance Applicative Stack where
  pure x = Stack [x]
  (Stack xs) <*> (Stack ys) = Stack (xs <*> ys)

instance Alternative Stack where
  empty = Stack []
  (<|>) (Stack xs) (Stack ys) = Stack (xs <|> ys)

-- | push to the stack
--
-- >>> push empty 0
-- Stack [0]
--
-- >>> push (push empty 0) 1
-- Stack [1,0]
push :: Stack a -> a -> Stack a
push (Stack xs) x = Stack (x:xs)

-- | pop an element from the stack
--
-- >>> pop (push empty 0)
-- Just (0,Stack [])
--
-- >>> pop (push (push empty 0) 1)
-- Just (1,Stack [0])
--
-- >>> pop empty
-- Nothing
pop :: Stack a -> Maybe (a, Stack a)
pop (Stack (x:xs)) = Just (x, Stack xs)
pop _ = Nothing

-- | get the element at the top of the stack
--
-- >>> top (push empty 'c')
-- Just 'c'
--
-- >>> top empty
-- Nothing
top :: Stack a -> Maybe a
top stk = fmap fst (pop stk)

-- | return the size of the stack
--
-- >>> size empty
-- 0
--
-- >>> size (push (push empty 0) 1)
-- 2
size :: Stack a -> Int
size (Stack l) = length l
