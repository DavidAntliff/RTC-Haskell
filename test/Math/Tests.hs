module Math.Tests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Math

unitTests :: TestTree
unitTests = testGroup "Math Unit Tests"
  [ testCase "Almost equal with default tolerance" $ assertBool [] $ (4.3 :: Double) `almostEqual` 4.3
  , testCase "Almost equal with default tolerance" $ assertBool [] $ not $ (4.3 :: Double) `almostEqual` (-4.3)
  , testCase "Almost equal with default tolerance" $ assertBool [] $ not $ (4.3 :: Double) `almostEqual` 4.300015
  , testCase "Almost equal with default tolerance" $ assertBool [] $ (4.3 :: Double) `almostEqual` 4.300005
  , testCase "Almost equal with specified tolerance" $ assertBool [] $ almostEqualTol (4.3 :: Double) 4.300005 0.00001
  , testCase "Almost equal with specified tolerance" $ assertBool [] $ almostEqualTol (4.3 :: Double) 4.25 0.1
  , testCase "Almost equal with specified tolerance" $ assertBool [] $ not $ almostEqualTol (4.3 :: Double) 4.31 0.001
  ]
