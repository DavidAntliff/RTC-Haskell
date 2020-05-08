module TestFoo (unitTests) where

import Foo

import Test.Tasty
--import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

unitTests :: TestTree
unitTests = testGroup "Foo Unit Tests"
  [ testCase "Test 1" $ assertEqual [] "OK" "OK"
  , testCase "Test 2" $ assertEqual [] "fail" "fail!"
  , testCase "Test Foo1" $ assertEqual [] 42 foo42
  ]
