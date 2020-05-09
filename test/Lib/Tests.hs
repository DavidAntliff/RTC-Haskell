module Lib.Tests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Lib

unitTests :: TestTree
unitTests = testGroup "Lib Unit Tests"
  [ testCase "Test Lib someFunc" $ assertEqual [] someFunc "someFunc"
  ]
