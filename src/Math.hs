{-# LANGUAGE ViewPatterns #-}
module Math ( AlmostEqual
            , almostEqual
            , almostEqualTol
            , radiansFromDegrees
            , degreesFromRadians
            ) where

import Data.Maybe (fromMaybe)

epsilon :: (Fractional a) => a
epsilon = 0.00001

class AlmostEqual a where
  almostEqual :: a -> a -> Bool

instance AlmostEqual Double where
  almostEqual x y = abs (x - y) < epsilon

instance AlmostEqual Float where
  almostEqual x y = abs (x - y) < epsilon

--almostEqual :: (Ord a, Fractional a) => a -> a -> Bool
--almostEqual x y = abs (x - y) < epsilon

almostEqualTol :: (Ord a, Fractional a) => a -> a -> a -> Bool
almostEqualTol x y tol = abs (x - y) < tol

-- default parameters using ViewPatterns: https://stackoverflow.com/posts/7781350/revisions
-- call with `almostEqualDef 1 2 Nothing` or `almostEqualDef 1 2 (Just 0.001)`
def :: a -> Maybe a -> a
def = fromMaybe

almostEqualDef :: (Ord a, Fractional a) => a -> a -> Maybe a -> Bool
almostEqualDef x y (def epsilon -> tol) = tol > abs (x - y)

radiansFromDegrees :: Floating a => a -> a
radiansFromDegrees = (*) (pi / 180.0)

degreesFromRadians :: Floating a => a -> a
degreesFromRadians = (*) (180.0 / pi)
