module RaySphere ( Ray
                 , origin
                 , direction
                 , ray
                 , position
                 , sphere
                 , Intersection(..)
                 , intersect
                 , IntersectionList
                 , intersections
                 , intersectionListCount
                 , hit
                 , transform
                 , getTransform
                 , setTransform
                 ) where

import Control.Exception
import Data.List (sortOn, find)

import Quadruple
import Matrix

data Ray = Ray { origin :: Quadruple     -- point
               , direction :: Quadruple  -- vector
               } deriving (Show)

ray :: Quadruple -> Quadruple -> Ray
ray o d = assert (isPoint o)
          $ assert (isVector d)
          $ Ray o d

position :: Ray -> Double -> Quadruple
position (Ray o d) t = o |+| d |* t

data Shape = Sphere Matrix44 | Obloid deriving (Eq, Show)

sphere :: Shape
sphere = Sphere (identity :: Matrix44)

data Intersection = Intersection { intersectionT :: Double
                                 , intersectionObject :: Shape
                                 } deriving (Eq, Show)

type IntersectionList = [Intersection]

-- # the vector from the sphere's center, to the ray origin
-- remember: the sphere is centered at the world origin
intersect :: Shape -> Ray -> IntersectionList
intersect s@(Sphere t) r
  | discriminant < 0 = []
  | otherwise = let t1 = (-b - sqrt discriminant) / (2 * a)
                    t2 = (-b + sqrt discriminant) / (2 * a)
                in intersections [ Intersection t1 s, Intersection t2 s ]
  where r' = transform r $ inverse t  -- transform the ray by the inverse of the sphere's transformation matrix
        sphere_to_ray = origin r' |-| point 0 0 0
        a = dot (direction r') (direction r')
        b = 2 * dot (direction r') sphere_to_ray
        c = dot sphere_to_ray sphere_to_ray - 1
        discriminant = b * b - 4 * a * c
intersect _ _ = []

sortByT :: IntersectionList -> IntersectionList
sortByT = sortOn intersectionT

intersections :: [Intersection] -> IntersectionList
intersections = id

intersectionListCount :: IntersectionList -> Int
intersectionListCount = length

-- return the Intersection with the lowest non-negative t value
hit :: IntersectionList -> Maybe Intersection
hit il = let sorted = sortByT il
         in find (\i -> intersectionT i >= 0) sorted

transform :: Ray -> Matrix44 -> Ray
transform (Ray o d) m = Ray (m |*| o) (m |*| d)

getTransform :: Shape -> Matrix44
getTransform (Sphere t) = t
getTransform _ = identity :: Matrix44

setTransform :: Shape -> Matrix44 -> Shape
setTransform (Sphere _) t = Sphere t
setTransform Obloid _ = Obloid
