{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Lish.Test.Parser
where

import Protolude

import Data.Fix
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

import Lish.Parser
import Lish.Types

parseTests :: [TestTree]
parseTests =
  [ testCase "simple commands" (simpleCommand "ls")
  , testCase "simple commands" (simpleCommand "atom")
  , testCase "simple commands" (simpleCommand "_foo")
  , testProperty "simple" propAtom
  ]

simpleCommand :: Text -> Assertion
simpleCommand t = parseCmd t @?= Right (Fix (Atom t))

propAtom :: [Char] -> Bool
propAtom s = s == "" ||
  fromMaybe '0' (head s) `elem` ("0123456789([])" :: [Char]) ||
  case s of
  "true" -> parseCmd t == Right (Fix (Bool True))
  "false" -> parseCmd t == Right (Fix (Bool False))
  _ -> parseCmd t == Right (Fix (Atom t))
  where t = toS s
