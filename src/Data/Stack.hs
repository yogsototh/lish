{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe              #-}

module Data.Stack
  ( Stack
  , pop
  , top
  , push
  , size
  )
  where

import           Protolude

-- | Stack data structure
data Stack a = Stack ![a] deriving (Eq,Show)

instance Functor Stack where
  fmap f (Stack xs) = Stack (fmap f xs)

instance Applicative Stack where
  pure x = Stack [x]
  (Stack xs) <*> (Stack ys) = Stack (xs <*> ys)

instance Alternative Stack where
  empty = Stack []
  (<|>) (Stack xs) (Stack ys) = Stack (xs <|> ys)

-- | O(1) Push to the stack
--
-- >>> push 0 empty
-- Stack [0]
--
-- >>> empty & push 0 & push 1
-- Stack [1,0]
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

-- | pop an element from the stack
--
-- >>> pop empty
-- Nothing
--
-- >>> pop (push 0 empty)
-- Just (0,Stack [])
--
-- >>> pop (empty & push 0 & push 1)
-- Just (1,Stack [0])
--
pop :: Stack a -> Maybe (a, Stack a)
pop (Stack (x:xs)) = Just (x, Stack xs)
pop _              = Nothing

-- | get the element at the top of the stack
--
-- >>> top (push 'c' empty)
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
-- >>> size (empty & push 0 & push 1)
-- 2
size :: Stack a -> Int
size (Stack l) = length l
