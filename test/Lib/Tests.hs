module Lib.Tests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Lib
import GHC.IO.Unsafe (unsafePerformIO)
import qualified Control.Exception (assert, AssertionFailed)
import Control.Monad

assertionException :: Selector Control.Exception.AssertionFailed
assertionException = const True

-- Hspec tests
spec_assertRaises :: Spec 
spec_assertRaises = do
  it "hello" $ 1 + 2 `shouldBe` 3
  it "goodbye" $ 1 - 2 `shouldNotBe` 3
  --it "assert" $ (liftM checkAssert) `shouldThrow` assertionException

unitTests :: TestTree
unitTests = testGroup "Lib Unit Tests"
  [ testCase "Test Lib someFunc" $ assertEqual [] someFunc "someFunc"
  --, testCase "Assert raises exception" $ assertRaises [] Control.Exception.AssertionFailed checkAssert 
  -- add Hspec tests to Tasty TestTree
  , unsafePerformIO (testSpec "spec" spec_assertRaises)
  ]
