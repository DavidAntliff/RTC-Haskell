{-# LANGUAGE FlexibleInstances #-}

module Quadruple ( Quadruple
                 , quadruple
                 , _impl
                 , x, y, z, w
                 , isPoint
                 , isVector
                 , point
                 , vector
                 , (|+|)
                 , (|-|)
                 , neg
                 , (|*)
                 , (*|)
                 , (|/)
                 , toList
                 , magnitude
                 , normalize
                 , dot
                 , cross
                 ) where

import Control.Lens
import Linear (V4 (V4), _x, _y, _z, _w, _xyz)
import qualified Linear (dot, norm, normalize, cross, vector)
import qualified Data.Foldable (toList)

import qualified Math (almostEqual, AlmostEqual)

-- Wrap Linear.V4
newtype Quadruple = Quadruple (V4 Double) deriving (Eq, Show, Read)

quadruple :: (Double, Double, Double, Double) -> Quadruple
quadruple (x, y, z, w) = Quadruple (V4 x y z w)

-- Get internal representation (for internal use only)
_impl :: Quadruple -> V4 Double
_impl (Quadruple q) = q

x :: Quadruple -> Double
x (Quadruple q) = q ^. _x

y :: Quadruple -> Double
y (Quadruple q) = q ^. _y

z :: Quadruple -> Double
z (Quadruple q) = q ^. _z

w :: Quadruple -> Double
w (Quadruple q) = q ^. _w

isPoint :: Quadruple -> Bool
isPoint q = w q == 1.0

isVector :: Quadruple -> Bool
isVector q = w q == 0.0

point :: Double -> Double -> Double -> Quadruple
point x' y' z' = Quadruple (V4 x' y' z' 1.0)

vector :: Double -> Double -> Double -> Quadruple
vector x' y' z' = Quadruple (V4 x' y' z' 0.0)

-- overriding (+) is problematic because it belongs to the Num class. Providing a Num instance for
-- Quadruple is possible but inconvenient. Better to use a new operator instead.
-- https://stackoverflow.com/a/8308197/143397
infixl 6 |+|  -- set same precedence and associativity as +
(|+|) :: Quadruple -> Quadruple -> Quadruple
(Quadruple q1) |+| (Quadruple q2) = Quadruple (q1 + q2)

infixl 6 |-|  -- set same precedence and associativity as -
(|-|) :: Quadruple -> Quadruple -> Quadruple
(Quadruple q1) |-| (Quadruple q2) = Quadruple (q1 - q2)

neg :: Quadruple -> Quadruple
neg (Quadruple q) = Quadruple (-q)

infixl 7 |*  -- set same precedence and associativity as *
(|*) :: Quadruple -> Double -> Quadruple
(Quadruple q) |* v = Quadruple (q * V4 v v v v)

infixl 7 *|  -- set same precedence and associativity as *
(*|) :: Double -> Quadruple -> Quadruple
v *| (Quadruple q) = Quadruple (V4 v v v v * q)

infixl 7 |/  -- set same precedence and associativity as /
(|/) :: Quadruple -> Double -> Quadruple
(Quadruple q) |/ v = Quadruple (q / V4 v v v v)

toList :: Quadruple -> [Double]
toList (Quadruple q) = Data.Foldable.toList q

magnitude :: Quadruple -> Double
magnitude (Quadruple q) = Linear.norm q

normalize :: Quadruple -> Quadruple
normalize (Quadruple q) = Quadruple (Linear.normalize q) 

dot :: Quadruple -> Quadruple -> Double
dot (Quadruple q1) (Quadruple q2) = q1 `Linear.dot` q2

-- 3D cross-product, ignore w
cross :: Quadruple -> Quadruple -> Quadruple
(Quadruple q1) `cross` (Quadruple q2) = Quadruple (Linear.vector $ Linear.cross (q1 ^. _xyz) (q2 ^. _xyz))

instance Math.AlmostEqual Quadruple where
  almostEqual (Quadruple (V4 x0 y0 z0 w0)) (Quadruple (V4 x1 y1 z1 w1)) 
    = x0 `Math.almostEqual` x1
      && y0 `Math.almostEqual` y1
      && z0 `Math.almostEqual` z1
      && w0 `Math.almostEqual` w1
