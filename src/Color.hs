module Color ( Color(..)
              , toList
              , (|+|)
              , (|-|)
              , (|*)
              , (*|)
              , (|*|)
              ) where

import qualified Math (almostEqual, AlmostEqual)

data Color = Color { red, green, blue :: Float } deriving (Eq, Show)

instance Math.AlmostEqual Color where
  almostEqual c1 c2 = and $ zipWith Math.almostEqual (toList c1) (toList c2)  

toList :: Color -> [Float]
toList (Color r g b) = [r, g, b]

infixl 6 |+|  -- set same precedence and associativity as +
(|+|) :: Color -> Color -> Color
(Color r1 g1 b1) |+| (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)

infixl 6 |-|  -- set same precedence and associativity as -
(|-|) :: Color -> Color -> Color
(Color r1 g1 b1) |-| (Color r2 g2 b2) = Color (r1 - r2) (g1 - g2) (b1 - b2)

infixl 7 |*  -- set same precedence and associativity as *
(|*) :: Color -> Float -> Color
(Color r g b) |* v = Color (r * v) (g * v) (b * v)

infixl 7 *|  -- set same precedence and associativity as *
(*|) :: Float -> Color -> Color
v *| (Color r g b) = Color (r * v) (g * v) (b * v)

-- Hadamard or Shur Product
infixl 7 |*|  -- set same precedence and associativity as *
(|*|) :: Color -> Color -> Color
(Color r1 g1 b1) |*| (Color r2 g2 b2) = Color (r1 * r2) (g1 * g2) (b1 * b2)
