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
                 ) where

import Control.Exception
import Control.Lens
import Data.List (sortOn, find)

import Quadruple

data Ray = Ray { origin :: Quadruple     -- point
               , direction :: Quadruple  -- vector
               } deriving (Show)

ray :: Quadruple -> Quadruple -> Ray
ray o d = assert (isPoint o)
          $ assert (isVector d)
          $ Ray o d

position :: Ray -> Double -> Quadruple
position (Ray o d) t = o |+| d |* t

data Shape = Sphere | Obloid deriving (Eq, Show)

sphere :: Shape
sphere = Sphere

data Intersection = Intersection { intersectionT :: Double
                                 , intersectionObject :: Shape
                                 } deriving (Eq, Show)

type IntersectionList = [Intersection]

-- # the vector from the sphere's center, to the ray origin
-- remember: the sphere is centered at the world origin
intersect :: Shape -> Ray -> IntersectionList
intersect s@Sphere r
  | discriminant < 0 = []
  | otherwise = let t1 = (-b - sqrt discriminant) / (2 * a)
                    t2 = (-b + sqrt discriminant) / (2 * a)
                in intersections [ Intersection t1 s, Intersection t2 s ]
  where sphere_to_ray = origin r |-| point 0 0 0
        a = dot (direction r) (direction r)
        b = 2 * dot (direction r) sphere_to_ray
        c = dot sphere_to_ray sphere_to_ray - 1
        discriminant = b * b - 4 * a * c
intersect _ r = []

sortByT :: IntersectionList -> IntersectionList
sortByT = sortOn intersectionT

intersections :: [Intersection] -> IntersectionList
intersections = id

intersectionListCount :: IntersectionList -> Int
intersectionListCount = length

-- return the Intersection with the lowest non-negative t value
hit :: IntersectionList -> Maybe Intersection
hit il = let sorted = sortByT il
         in find (\x -> intersectionT x >= 0) sorted
