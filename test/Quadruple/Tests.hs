module Quadruple.Tests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Quadruple
import Math (almostEqual)

unitTests :: TestTree

unitTests = testGroup "All Unit Tests"
  [ testGroup "A 3-Vector has x, y and z components" $
    let q = Quadruple 4.3 (-4.2) 3.1 1.0 in
    [ testCase "x" $ assertEqual [] (x q) 4.3
    , testCase "y" $ assertEqual [] (y q) (-4.2)
    , testCase "z" $ assertEqual [] (z q) 3.1
    , testCase "w" $ assertEqual [] (w q) 1.0
    ]
  ]

