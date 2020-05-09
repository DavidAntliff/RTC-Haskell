{-# LANGUAGE ViewPatterns #-}
module Math ( almostEqual
            , almostEqualTol
            ) where

import Data.Maybe (fromMaybe)

epsilon :: (Fractional a) => a
epsilon = 0.00001

almostEqual :: (Ord a, Fractional a) => a -> a -> Bool
almostEqual x y = abs (x - y) < epsilon

almostEqualTol :: (Ord a, Fractional a) => a -> a -> a -> Bool
almostEqualTol x y tol = abs (x - y) < tol

-- default parameters using ViewPatterns: https://stackoverflow.com/posts/7781350/revisions
-- call with `almostEqualDef 1 2 Nothing` or `almostEqualDef 1 2 (Just 0.001)`
def :: a -> Maybe a -> a
def = fromMaybe

almostEqualDef :: (Ord a, Fractional a) => a -> a -> Maybe a -> Bool
almostEqualDef x y (def epsilon -> tol) = tol > abs (x - y)

