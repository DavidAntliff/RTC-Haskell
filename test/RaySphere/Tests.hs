module RaySphere.Tests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Lens

import RaySphere
import qualified RaySphere (transform)
import Quadruple
import Transformations
import Math (almostEqual)
import Matrix

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
      in [ testCase "count" $ assertEqual [] 2 (intersectionListCount xs)
         , testCase "first" $ assertEqual [] 4.0 (intersectionT $ xs ^?! element 0)
         , testCase "second" $ assertEqual [] 6.0 (intersectionT $ xs ^?! element 1)
         ] 
  , testGroup "A ray intersects a sphere at a tangent" $
      let r = ray (point 0 1 (-5)) (vector 0 0 1)
          s = sphere
          xs = intersect s r
      in [ testCase "count" $ assertEqual [] 2 (intersectionListCount xs)
         , testCase "first" $ assertEqual [] 5.0 (intersectionT $ xs ^?! element 0) 
         , testCase "second" $ assertEqual [] 5.0 (intersectionT $ xs ^?! element 1)
         ] 
  , testCase "A ray misses a sphere" $
      let r = ray (point 0 2 (-5)) (vector 0 0 1)
          s = sphere
          xs = intersect s r
      in assertEqual [] (length xs) 0
  , testGroup "An intersection encapsulates t and object" $
      let s = sphere
          i = Intersection 3.5 s
      in [ testCase "t" $ assertEqual [] 3.5 (intersectionT i)
         , testCase "object" $ assertEqual [] s (intersectionObject i) 
         ]
  , testGroup "Aggregating intersections" $
      let s = sphere
          i1 = Intersection 1 s
          i2 = Intersection 2 s
          xs = intersections [i1, i2]
      in [ testCase "t" $ assertEqual [] 2 (intersectionListCount xs)
         , testCase "first" $ assertEqual [] 1 (intersectionT $ xs ^?! element 0) 
         , testCase "second" $ assertEqual [] 2 (intersectionT $ xs ^?! element 1) 
         ]
  , testGroup "Intersect sets the object on the intersection" $
      let r = ray (point 0 0 (-5)) (vector 0 0 1)
          s = sphere
          xs = intersect s r
      in [ testCase "t" $ assertEqual [] 2 (intersectionListCount xs)
         , testCase "first" $ assertEqual [] s (intersectionObject $ xs ^?! element 0) 
         , testCase "second" $ assertEqual [] s (intersectionObject $ xs ^?! element 1) 
         ]
  , testCase "The hit, when all intersections have positive t" $
      let s = sphere
          i1 = Intersection 1 s
          i2 = Intersection 2 s
          xs = intersections [i2, i1]
      in assertEqual [] (hit xs) (Just i1)
  , testCase "The hit, when some intersections have negative t" $
      let s = sphere
          i1 = Intersection (-1) s
          i2 = Intersection 1 s
          xs = intersections [i2, i1]
      in assertEqual [] (hit xs) (Just i2)  
  , testCase "The hit, when all intersections have negative t" $
      let s = sphere
          i1 = Intersection (-2) s
          i2 = Intersection (-1) s
          xs = intersections [i2, i1]
      in assertEqual [] (hit xs) Nothing  
  , testCase "The hit is always the lowest non-negative intersection" $
      let s = sphere
          i1 = Intersection 5 s
          i2 = Intersection 7 s
          i3 = Intersection (-3) s
          i4 = Intersection 2 s
          xs = intersections [i1, i2, i3, i4]
      in assertEqual [] (hit xs) (Just i4)
  , testGroup "Translating a ray" $
      let r = ray (point 1 2 3) (vector 0 1 0)
          m = translation 3 4 5
          r2 = RaySphere.transform r m
      in [ testCase "origin" $ assertEqual [] (origin r2) (point 4 6 8)
         , testCase "direction" $ assertEqual [] (direction r2) (vector 0 1 0)
         ]
  , testGroup "Scaling a ray" $
      let r = ray (point 1 2 3) (vector 0 1 0)
          m = scaling 2 3 4
          r2 = RaySphere.transform r m
      in [ testCase "origin" $ assertEqual [] (origin r2) (point 2 6 12)
         , testCase "direction" $ assertEqual [] (direction r2) (vector 0 3 0)
         ]
  , testCase "A sphere's default transformation" $
      let s = sphere
      in assertEqual [] (getTransform s) (identity :: Matrix44)
  , testCase "Changing a sphere's transformation" $
      let s = sphere
          t = translation 2 3 4
          s' = setTransform s t
      in assertEqual [] (getTransform s') t
  , testGroup "Intersecting a scaled sphere with a ray" $
      let r = ray (point 0 0 (-5)) (vector 0 0 1)
          s = sphere
          s' = setTransform s $ scaling 2 2 2
          xs = intersect s' r
      in [ testCase "count" $ assertEqual [] 2 (intersectionListCount xs)
         , testCase "xs[0].t" $ assertEqual [] 3 (intersectionT $ xs ^?! element 0)
         , testCase "xs[0].t" $ assertEqual [] 7 (intersectionT $ xs ^?! element 1)
         ]
  , testCase "Intersecting a translated sphere with a ray" $
      let r = ray (point 0 0 (-5)) (vector 0 0 1)
          s = sphere
          s' = setTransform s $ translation 5 0 0
          xs = intersect s' r
      in assertEqual [] 0 (intersectionListCount xs)
  ]

