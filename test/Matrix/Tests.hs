module Matrix.Tests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Matrix
import Quadruple
import Math (almostEqual)

unitTests :: TestTree
unitTests = testGroup "Matrix Unit Tests"
  [ testGroup "Constructing and inspecting a 4x4 matrix" $
      let m = matrix44 ((1, 2, 3, 4), (5.5, 6.5, 7.5, 8.5), (9, 10, 11, 12), (13.5, 14.5, 15.5, 16.5))
      in [ testCase "M[0,0]" $ assertEqual [] (elementAt (0, 0) m ) 1.0
         , testCase "M[0,3]" $ assertEqual [] (elementAt (0, 3) m ) 4.0
         , testCase "M[1,0]" $ assertEqual [] (elementAt (1, 0) m ) 5.5
         , testCase "M[1,2]" $ assertEqual [] (elementAt (1, 2) m ) 7.5
         , testCase "M[2,2]" $ assertEqual [] (elementAt (2, 2) m ) 11
         , testCase "M[3,0]" $ assertEqual [] (elementAt (3, 0) m ) 13.5
         , testCase "M[3,2]" $ assertEqual [] (elementAt (3, 2) m ) 15.5
         ]
  , testGroup "Constructing and inspecting a 2x2 matrix" $
      let m = matrix22 ((-3, 5), (1, -2))
      in [ testCase "M[0,0]" $ assertEqual [] (elementAt (0, 0) m ) (-3)
         , testCase "M[0,1]" $ assertEqual [] (elementAt (0, 1) m ) 5
         , testCase "M[1,0]" $ assertEqual [] (elementAt (1, 0) m ) 1
         , testCase "M[1,1]" $ assertEqual [] (elementAt (1, 1) m ) (-2)
         ]
  , testGroup "Constructing and inspecting a 3x3 matrix" $
      let m = matrix33 ((-3, 5, 0), (1, -2, -7), (0, 1, 1))
      in [ testCase "M[0,0]" $ assertEqual [] (elementAt (0, 0) m ) (-3)
         , testCase "M[1,1]" $ assertEqual [] (elementAt (1, 1) m ) (-2)
         , testCase "M[2,2]" $ assertEqual [] (elementAt (2, 2) m ) 1
         ]
  , testCase "Matrix 4x4 equality with identical matrices" $
      let a = matrix44 ((1,2,3,4), (5,6,7,8), (9,8,7,6), (5,4,3,2))
          b = matrix44 ((1,2,3,4), (5,6,7,8), (9,8,7,6), (5,4,3,2))
      in assertBool [] $ almostEqual a b
  , testCase "Matrix 4x4 exact equality with identical matrices" $
      let a = matrix44 ((1,2,3,4), (5,6,7,8), (9,8,7,6), (5,4,3,2))
          b = matrix44 ((1,2,3,4), (5,6,7,8), (9,8,7,6), (5,4,3,2))
      in assertEqual [] a b
  , testCase "Matrix 4x4 equality with different matrices" $
      let a = matrix44 ((1,2,3,4), (5,6,7,8), (9,8,7,6), (5,4,3,2))
          b = matrix44 ((1,2,3,4), (5,6,7,8), (9,8,7,6), (5,4,3.5,2))
      in assertBool [] $ not $ almostEqual a b
  , testCase "Matrix 3x3 equality with identical matrices" $
      let a = matrix33 ((1,2,3), (5,6,7), (8,9,0))
          b = matrix33 ((1,2,3), (5,6,7), (8,9,0))
      in assertBool [] $ almostEqual a b
  , testCase "Matrix 3x3 exact equality with identical matrices" $
      let a = matrix33 ((1,2,3), (5,6,7), (8,9,0))
          b = matrix33 ((1,2,3), (5,6,7), (8,9,0))
      in assertEqual [] a b
  , testCase "Matrix 3x3 equality with different matrices" $
      let a = matrix33 ((1,2,3), (5,6,7.1), (8,9,0))
          b = matrix33 ((1,2,3), (5,6,7), (8,9,0))
      in assertBool [] $ not $ almostEqual a b
  , testCase "Matrix 2x2 equality with identical matrices" $
      let a = matrix22 ((1,2), (5,6))
          b = matrix22 ((1,2), (5,6))
      in assertBool [] $ almostEqual a b
  , testCase "Matrix 2x2 exact equality with identical matrices" $
      let a = matrix22 ((1,2), (5,6))
          b = matrix22 ((1,2), (5,6))
      in assertEqual [] a b
  , testCase "Matrix 2x2 equality with different matrices" $
      let a = matrix22 ((1,2), (5,6))
          b = matrix22 ((1.05,2), (5,6))
      in assertBool [] $ not $ almostEqual a b
  , testCase "Multiplying two 4x4 matrices" $
      let a = matrix44 ((1,2,3,4), (5,6,7,8), (9,8,7,6), (5,4,3,2))
          b = matrix44 ((-2,1,2,3), (3,2,1,-1), (4,3,6,5), (1,2,7,8))
          c = matrix44 ((20,22,50,48), (44,54,114,108), (40,58,110,102), (16,26,46,42))
      in assertBool [] $ almostEqual (a |*| b) c
  , testCase "Multiplying two 3x3 matrices" $
      let a = matrix33 ((1,2,3), (5,6,7), (9,8,7))
          b = matrix33 ((-2,1,2), (3,2,1), (4,3,6))
          c = matrix33 ((16,14,22), (36,38,58), (34,46,68))
      in assertBool [] $ almostEqual (a |*| b) c
  , testCase "Multiplying two 2x2 matrices" $
      let a = matrix22 ((1,2), (5,6))
          b = matrix22 ((-2,1), (3,2))
          c = matrix22 ((4,5), (8,17))
      in assertBool [] $ almostEqual (a |*| b) c
  , testCase "A 4x4 matrix multiplied by a quadruple" $
      let a = matrix44 ((1,2,3,4), (2,4,4,2), (8,6,4,1), (0,0,0,1))
          b = Quadruple 1.0 2.0 3.0 1.0
      in assertBool [] $ almostEqual (a |*| b) (Quadruple 18.0 24.0 33.0 1.0)
  , testCase "Multiplying a 4x4 matrix by the identity matrix" $
      let a = matrix44 ((0,1,2,4), (1,2,4,8), (2,4,8,16), (4,8,16,32))
          i = identity :: Matrix44
      in assertBool [] $ almostEqual (a |*| i) a
  , testCase "Multiplying a 3x3 matrix by the identity matrix" $
      let a = matrix33 ((0,1,2), (1,2,4), (2,4,8))
          i = identity :: Matrix33
      in assertBool [] $ almostEqual (a |*| i) a
  , testCase "Multiplying a 2x2 matrix by the identity matrix" $
      let a = matrix22 ((0,1), (1,2))
          i = identity :: Matrix22
      in assertBool [] $ almostEqual (a |*| i) a
  , testCase "Multiplying the identity matrix by a tuple" $
      let q = Quadruple 1 2 3 4
          i = identity :: Matrix44
      in assertBool [] $ almostEqual (i |*| q) q
  , testCase "Transposing a 4x4 matrix" $
      let a = matrix44 ((0,9,3,0), (9,8,0,8), (1,8,5,3), (0,0,5,8))
          b = matrix44 ((0,9,1,0), (9,8,8,0), (3,0,5,5), (0,8,3,8))
      in assertBool [] $ almostEqual (transpose a) b
  , testCase "Transposing a 3x3 matrix" $
      let a = matrix33 ((0,9,3), (9,8,0), (1,8,5))
          b = matrix33 ((0,9,1), (9,8,8), (3,0,5))
      in assertBool [] $ almostEqual (transpose a) b
  , testCase "Transposing a 2x2 matrix" $
      let a = matrix22 ((0,9), (9,8))
          b = matrix22 ((0,9), (9,8))
      in assertBool [] $ almostEqual (transpose a) b
  ]
