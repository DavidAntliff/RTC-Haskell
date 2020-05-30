module Transformations.Tests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Quadruple
import Matrix
import Transformations
import Math (almostEqual)

unitTests :: TestTree

unitTests = testGroup "Transformations Unit Tests"
  [ testCase "Multiplying by a translation matrix" $
      let transform = translation 5 (-3) 2
          p = point (-3) 4 5
      in assertBool [] $ almostEqual (transform |*| p) (point 2 1 7)
  , testCase "Multiplying by the inverse of a translation matrix" $
        let transform = translation 5 (-3) 2
            inv = inverse transform
            p = point (-3) 4 5
        in assertBool [] $ almostEqual (inv |*| p) (point (-8) 7 3)
  , testCase "Translation does not affect vectors" $
        let transform = translation 5 (-3) 2
            v = vector (-3) 4 5
        in assertBool [] $ almostEqual (transform |*| v) v
  , testCase "A scaling matrix applied to a point" $
        let transform = scaling 2 3 4
            p = point (-4) 6 8
        in assertBool [] $ almostEqual (transform |*| p) (point (-8) 18 32)
  , testCase "A scaling matrix applied to a vector" $
        let transform = scaling 2 3 4
            v = vector (-4) 6 8
        in assertBool [] $ almostEqual (transform |*| v) (vector (-8) 18 32)
  , testCase "Multiplying by the inverse of a scaling matrix" $
        let transform = scaling 2 3 4
            inv = inverse transform
            v = vector (-4) 6 8
        in assertBool [] $ almostEqual (inv |*| v) (vector (-2) 2 2)
  , testCase "Reflection is scaling by a negative value" $
        let transform = scaling (-1) 1 1
            p = point 2 3 4
        in assertBool [] $ almostEqual (transform |*| p) (point (-2) 3 4)
  , testCase "Reflection is scaling by a negative value" $
        let transform = scaling (-1) 1 1
            p = point 2 3 4
        in assertBool [] $ almostEqual (transform |*| p) (point (-2) 3 4)
  , testGroup "Rotating a point around the x axis" $
        let p = point 0 1 0
            half_quarter = rotationX $ pi / 4.0
            full_quarter = rotationX $ pi / 2.0
        in [ testCase "half quarter" $ assertBool [] $ almostEqual (half_quarter |*| p) (point 0 (sqrt 2 / 2) (sqrt 2 / 2))
           , testCase "full quarter" $ assertBool [] $ almostEqual (full_quarter |*| p) (point 0 0 1)
           ] 
  , testCase "The inverse of an x-rotation rotates in the opposite direction" $
        let p = point 0 1 0
            half_quarter = rotationX $ pi / 4.0
            inv = inverse half_quarter
        in assertBool [] $ almostEqual (inv |*| p) (point 0 (sqrt 2 / 2) (-sqrt 2 / 2))
  , testGroup "Rotating a point around the y axis" $
        let p = point 0 0 1
            half_quarter = rotationY $ pi / 4.0
            full_quarter = rotationY $ pi / 2.0
        in [ testCase "half quarter" $ assertBool [] $ almostEqual (half_quarter |*| p) (point (sqrt 2 / 2) 0 (sqrt 2 / 2))
           , testCase "full quarter" $ assertBool [] $ almostEqual (full_quarter |*| p) (point 1 0 0)
           ] 
  , testGroup "Rotating a point around the z axis" $
        let p = point 0 1 0
            half_quarter = rotationZ $ pi / 4.0
            full_quarter = rotationZ $ pi / 2.0
        in [ testCase "half quarter" $ assertBool [] $ almostEqual (half_quarter |*| p) (point (-sqrt 2 / 2) (sqrt 2 / 2) 0)
           , testCase "full quarter" $ assertBool [] $ almostEqual (full_quarter |*| p) (point (-1) 0 0)
           ] 
  , testCase "A shearing transformation moves x in proportion to y" $
        let transform = shearing 1 0 0 0 0 0
            p = point 2 3 4
        in assertBool [] $ almostEqual (transform |*| p) (point 5 3 4)
  , testCase "A shearing transformation moves x in proportion to z" $
        let transform = shearing 0 1 0 0 0 0
            p = point 2 3 4
        in assertBool [] $ almostEqual (transform |*| p) (point 6 3 4)
  , testCase "A shearing transformation moves y in proportion to x" $
        let transform = shearing 0 0 1 0 0 0
            p = point 2 3 4
        in assertBool [] $ almostEqual (transform |*| p) (point 2 5 4)
  , testCase "A shearing transformation moves y in proportion to z" $
        let transform = shearing 0 0 0 1 0 0
            p = point 2 3 4
        in assertBool [] $ almostEqual (transform |*| p) (point 2 7 4)
  , testCase "A shearing transformation moves z in proportion to x" $
        let transform = shearing 0 0 0 0 1 0
            p = point 2 3 4
        in assertBool [] $ almostEqual (transform |*| p) (point 2 3 6)
  , testCase "A shearing transformation moves z in proportion to y" $
        let transform = shearing 0 0 0 0 0 1
            p = point 2 3 4
        in assertBool [] $ almostEqual (transform |*| p) (point 2 3 7)
  , testGroup "Individual transformations are applied in sequence" $
        let p = point 1 0 1
            a = rotationX $ pi / 2
            b = scaling 5 5 5
            c = translation 10 5 7
            p2 = a |*| p
            p3 = b |*| p2
            p4 = c |*| p3
        in [ testCase "rotation first" $ assertBool [] $ almostEqual p2 (point 1 (-1) 0)
           , testCase "then scaling" $ assertBool [] $ almostEqual p3 (point 5 (-5) 0)
           , testCase "then translation" $ assertBool [] $ almostEqual p4 (point 15 0 7)
           ] 
  , testCase "Chained transformations must be applied in reverse order" $
        let p = point 1 0 1
            a = rotationX $ pi / 2
            b = scaling 5 5 5
            c = translation 10 5 7
            t = c |*| b |*| a
        in assertBool [] $ almostEqual (t |*| p) (point 15 0 7)
  ]
