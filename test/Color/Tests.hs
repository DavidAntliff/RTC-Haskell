module Color.Tests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Color
import Math (almostEqual, AlmostEqual)

unitTests :: TestTree

unitTests = testGroup "Color Tnit Tests"
  [ testGroup "Colors are (red, green, blue) tuples" $
      let c = Color (-0.5) 0.4 1.7 in
        [ testCase "red" $ assertEqual [] (red c) (-0.5)
        , testCase "green" $ assertEqual [] (green c) 0.4
        , testCase "blue" $ assertEqual [] (blue c) 1.7
        ]
  , testCase "Adding colors" $
      let c1 = Color 0.9 0.6 0.75
          c2 = Color 0.7 0.1 0.25
      in assertBool [] $ almostEqual (c1 |+| c2) (Color 1.6 0.7 1.0)
  , testCase "Subtracting colors" $
      let c1 = Color 0.9 0.6 0.75
          c2 = Color 0.7 0.1 0.25
      in assertBool [] $ almostEqual (c1 |-| c2) (Color 0.2 0.5 0.5)
  , testGroup "Multiplying a color" $
      let c = Color 0.2 0.3 0.4
      in [ testCase "by a scalar" $ assertBool [] $ almostEqual (c |* 2) (Color 0.4 0.6 0.8)
         , testCase "by a scalar prefix" $ assertBool [] $ almostEqual (2 *| c) (Color 0.4 0.6 0.8)
         ]  
  , testCase "Multiplying colors" $
      let c1 = Color 1 0.2 0.4
          c2 = Color 0.9 1 0.1
      in assertBool [] $ almostEqual (c1 |*| c2) (Color 0.9 0.2 0.04)
  ]


