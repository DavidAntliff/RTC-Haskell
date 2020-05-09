module Quadruple.Tests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Quadruple
import Math (almostEqual)

unitTests :: TestTree

unitTests = testGroup "features/tuples.feature"
  [ testGroup "A tuple with w=1.0 is a point" $
      let q = Quadruple 4.3 (-4.2) 3.1 1.0 in
        [ testCase "x" $ assertEqual [] (x q) 4.3
        , testCase "y" $ assertEqual [] (y q) (-4.2)
        , testCase "z" $ assertEqual [] (z q) 3.1
        , testCase "w" $ assertEqual [] (w q) 1.0
        , testCase "is a point" $ assertBool [] $ isPoint q
        , testCase "is not a vector" $ assertBool [] $ not $ isVector q
        ]
  , testGroup "A tuple with w=0 is a vector" $
      let q = Quadruple 4.3 (-4.2) 3.1 0.0 in
        [ testCase "x" $ assertEqual [] (x q) 4.3
        , testCase "y" $ assertEqual [] (y q) (-4.2)
        , testCase "z" $ assertEqual [] (z q) 3.1
        , testCase "w" $ assertEqual [] (w q) 0.0
        , testCase "is a point" $ assertBool [] $ not $ isPoint q
        , testCase "is not a vector" $ assertBool [] $ isVector q
        ]
  , testCase "point() creates tuples with w=1" $
      assertEqual [] (point 4 (- 4) 3) (Quadruple 4 (- 4) 3 1)
  , testCase "vector() creates tuples with w=0" $
      assertEqual [] (vector 4 (- 4) 3) (Quadruple 4 (- 4) 3 0)
  , testCase "Adding two tuples" $
      let a1 = Quadruple 3 (-2) 5 1
          a2 = Quadruple (-2) 3 1 0
      in assertEqual [] (a1 |+| a2) (Quadruple 1 1 6 1)      
  , testCase "Subtracting two points" $
      let p1 = point 3 2 1
          p2 = point 5 6 7
      in assertEqual [] (p1 |-| p2) (vector (-2) (-4) (-6))      
  , testCase "Subtracting a vector from a point" $
      let p = point 3 2 1
          v = vector 5 6 7
      in assertEqual [] (p |-| v) (point (-2) (-4) (-6))      
  , testCase "Subtracting two vectors" $
      let v1 = vector 3 2 1
          v2 = vector 5 6 7
      in assertEqual [] (v1 |-| v2) (vector (-2) (-4) (-6))   
  , testCase "Subtracting a vector from the zero vector" $
      let zero = vector 0 0 0
          v = vector 1 (-2) 3
      in assertEqual [] (zero |-| v) (vector (-1) 2 (-3))   
  , testCase "Negating a tuple" $
      let a = Quadruple 1 (-2) 3 (-4)
      in assertEqual [] (neg a) (Quadruple (-1) 2 (-3) 4)   
  , testGroup "Multiplying a tuple" $
      let a = Quadruple 1 (-2) 3 (-4)
      in [ testCase "by a scalar" $ assertEqual [] (a |* 3.5) (Quadruple 3.5 (-7) 10.5 (-14)) 
         , testCase "by a scalar prefix" $ assertEqual [] (3.5 *| a) (Quadruple 3.5 (-7) 10.5 (-14)) 
         , testCase "by a fraction" $ assertEqual [] (a |* 0.5) (Quadruple 0.5 (-1) 1.5 (-2)) 
         , testCase "by a fraction prefix" $ assertEqual [] (0.5 *| a) (Quadruple 0.5 (-1) 1.5 (-2)) 
         ]  
  , testCase "Dividing a tuple by a scalar" $
      let a = Quadruple 1 (-2) 3 (-4)
      in assertEqual [] (a |/ 2) (Quadruple 0.5 (-1) 1.5 (-2))
  , testCase "convert tuple to list" $
      assertEqual [] (toList $ Quadruple 1 2 3 4) [1, 2, 3, 4]
  , testCase "Computing the magnitude of vector(1, 0, 0)" $
      let v = vector 1 0 0
      in assertEqual [] (magnitude v) 1   
  , testCase "Computing the magnitude of vector(0, 1, 0)" $
      let v = vector 0 1 0
      in assertEqual [] (magnitude v) 1   
  , testCase "Computing the magnitude of vector(0, 0, 1)" $
      let v = vector 0 0 1
      in assertEqual [] (magnitude v) 1   
  , testCase "Computing the magnitude of vector(1, 2, 3)" $
      let v = vector 1 2 3
      in assertEqual [] (magnitude v) (sqrt 14)   
  , testCase "Computing the magnitude of vector(-1, -2, -3)" $
      let v = vector (-1) (-2) (-3)
      in assertEqual [] (magnitude v) (sqrt 14)   
  , testCase "Normalizing vector(4, 0, 0) gives vector(1, 0, 0)" $
      assertEqual [] (normalize $ vector 4 0 0) (vector 1 0 0)   
  , testCase "Normalizing vector(1, 2, 3)" $
      let v = normalize $ vector 1 2 3
      in assertBool [] $ and $ zipWith almostEqual (toList v) [0.26726, 0.53452, 0.80178, 0.0]   
  , testCase "The magnitude of a normalized vector" $
      assertBool [] $ magnitude (normalize $ vector 1 2 3) `almostEqual` 1.0 
  , testCase "The dot product of two tuples" $
      let a = vector 1 2 3
          b = vector 2 3 4
      in assertEqual [] (dot a b) 20.0
  , testGroup "The cross product of two vectors" $ 
      let a = vector 1 2 3
          b = vector 2 3 4
      in [ testCase "a cross b" $ assertEqual [] (a `cross` b) (vector (-1) 2 (-1))
         , testCase "b cross a" $ assertEqual [] (b `cross` a) (vector 1 (-2) 1)
         ]
  ]

