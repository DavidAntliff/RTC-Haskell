module RaySphere ( Ray
                 , origin
                 , direction
                 , ray
                 , position
                 , sphere
                 , intersect
                 ) where

import Control.Exception

import Quadruple

data Ray = Ray { origin :: Quadruple     -- point
               , direction :: Quadruple  -- vector
               } deriving (Show)

data Sphere = Sphere deriving (Show)

ray :: Quadruple -> Quadruple -> Ray
ray o d = assert (isPoint o)
          $ assert (isVector d)
          $ Ray o d

position :: Ray -> Double -> Quadruple
position (Ray o d) t = o |+| d |* t

sphere :: Sphere
sphere = Sphere

-- # the vector from the sphere's center, to the ray origin
-- remember: the sphere is centered at the world origin
intersect :: Sphere -> Ray -> [Double]
intersect s r
  | discriminant < 0 = []
  | otherwise = let t1 = (-b - sqrt discriminant) / (2 * a)
                    t2 = (-b + sqrt discriminant) / (2 * a)
                in [t1, t2]
  where sphere_to_ray = origin r |-| point 0 0 0
        a = dot (direction r) (direction r)
        b = 2 * dot (direction r) sphere_to_ray
        c = dot sphere_to_ray sphere_to_ray - 1
        discriminant = b * b - 4 * a * c
