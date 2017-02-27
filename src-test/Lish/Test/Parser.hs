{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Lish.Test.Parser
where

import Protolude
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

import Lish.Parser
import Lish.Types

parseTests :: [TestTree]
parseTests =
  [ testCase "simple commands" simpleCommand ]

simpleCommand :: Assertion
simpleCommand = parseCmd "ls" @?= Right (Atom "ls")
