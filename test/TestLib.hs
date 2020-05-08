module TestLib (unitTests) where

import Lib

import Test.Tasty
--import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Lib Unit Tests"
  [ testCase "Test Lib 1" $ assertEqual [] someFunc "someFunc"
  ]
