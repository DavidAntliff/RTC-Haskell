module RaySphere.Tests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit

import RaySphere
import Quadruple
import Math (almostEqual)

unitTests :: TestTree

unitTests = testGroup "Ray-Sphere Unit Tests"
  [ testGroup "Creating and querying a ray" $
      let origin' = point 1 2 3
          direction' = vector 4 5 6
          r = ray origin' direction'
      in [ testCase "origin" $ assertEqual [] (origin r) origin'
         , testCase "direction" $ assertEqual [] (direction r) direction' 
         ]
  , testGroup "Computing a point from a distance" $
      let r = ray (point 2 3 4) (vector 1 0 0)
      in [ testCase "t = 0" $ assertBool [] $ almostEqual (position r 0.0) (point 2 3 4)
         , testCase "t = 1" $ assertBool [] $ almostEqual (position r 1.0) (point 3 3 4)
         , testCase "t = -1" $ assertBool [] $ almostEqual (position r (-1.0)) (point 1 3 4)
         , testCase "t = 2.5" $ assertBool [] $ almostEqual (position r 2.5) (point 4.5 3 4)
         ]
  , testGroup "A ray intersects a sphere at two points" $
      let r = ray (point 0 0 (-5)) (vector 0 0 1)
          s = sphere
          xs = intersect s r
      in [ testCase "count" $ assertEqual [] (length xs) 2
         , testCase "first" $ assertEqual [] (head xs) 4.0 
         , testCase "second" $ assertEqual [] (head . tail $ xs) 6.0
         ] 
  , testGroup "A ray intersects a sphere at a tangent" $
      let r = ray (point 0 1 (-5)) (vector 0 0 1)
          s = sphere
          xs = intersect s r
      in [ testCase "count" $ assertEqual [] (length xs) 2
         , testCase "first" $ assertEqual [] (head xs) 5.0 
         , testCase "second" $ assertEqual [] (head . tail $ xs) 5.0
         ] 
  , testCase "A ray misses a sphere" $
      let r = ray (point 0 2 (-5)) (vector 0 0 1)
          s = sphere
          xs = intersect s r
      in assertEqual [] (length xs) 0
  ]

