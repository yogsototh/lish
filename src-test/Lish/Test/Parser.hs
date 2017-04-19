{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Lish.Test.Parser
where

import Protolude

import Prelude (String)
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
  , testCase "multiline command"
             (parseCmd "(fn [x] ; comment \n   (+ x 1))" @?= Right incExpr)
  , testProperty "simple" propAtom
  ]

incExpr :: Expr
incExpr = Fix (Lambda [Fix (Atom "fn")
                      ,Fix (List [Fix (Atom "x")])
                      ,Fix (Lambda [Fix (Atom "+")
                                   ,Fix (Atom "x")
                                   ,Fix (Num 1)])])

simpleCommand :: Text -> Assertion
simpleCommand t = parseCmd t @?= Right (Fix (Atom t))

propAtom :: String -> Bool
propAtom s = s == "" ||
  fromMaybe '0' (head s) `elem` ("0123456789([])" :: String) ||
  case s of
  "true" -> parseCmd t == Right (Fix (Bool True))
  "false" -> parseCmd t == Right (Fix (Bool False))
  _ -> parseCmd t == Right (Fix (Atom t))
  where t = toS s
