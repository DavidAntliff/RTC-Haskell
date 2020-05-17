module Matrix.Tests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Matrix

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
  ]
