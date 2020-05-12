module Quadruple ( Quadruple(..)
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

data Quadruple = Quadruple { x, y, z, w :: Double } deriving (Eq, Show, Read)

isPoint :: Quadruple -> Bool
isPoint q = w q == 1.0

isVector :: Quadruple -> Bool
isVector q = w q == 0.0

point :: Double -> Double -> Double -> Quadruple
point x' y' z' = Quadruple x' y' z' 1.0

vector :: Double -> Double -> Double -> Quadruple
vector x' y' z' = Quadruple x' y' z' 0.0

-- overridding (+) is problematic because it belongs to the Num class. Providing a Num instance for
-- Quadruple is possible but inconvenient. Better to use a new operator instead.
-- https://stackoverflow.com/a/8308197/143397
infixl 6 |+|  -- set same precedence and associativity as +
(|+|) :: Quadruple -> Quadruple -> Quadruple
(Quadruple x1 y1 z1 w1) |+| (Quadruple x2 y2 z2 w2) = Quadruple (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)

infixl 6 |-|  -- set same precedence and associativity as -
(|-|) :: Quadruple -> Quadruple -> Quadruple
(Quadruple x1 y1 z1 w1) |-| (Quadruple x2 y2 z2 w2) = Quadruple (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)

neg :: Quadruple -> Quadruple
neg (Quadruple x' y' z' w') = Quadruple (negate x') (negate y') (negate z') (negate w')

infixl 7 |*  -- set same precedence and associativity as *
(|*) :: Quadruple -> Double -> Quadruple
(Quadruple x' y' z' w') |* v = Quadruple (x' * v) (y' * v) (z' * v) (w' * v)

infixl 7 *|  -- set same precedence and associativity as *
(*|) :: Double -> Quadruple -> Quadruple
v *| (Quadruple x' y' z' w') = Quadruple (x' * v) (y' * v) (z' * v) (w' * v)

infixl 7 |/  -- set same precedence and associativity as /
(|/) :: Quadruple -> Double -> Quadruple
(Quadruple x' y' z' w') |/ v = Quadruple (x' / v) (y' / v) (z' / v) (w' / v)

toList :: Quadruple -> [Double]
toList (Quadruple x' y' z' w') = [x', y', z', w']

magnitude :: Quadruple -> Double
magnitude = sqrt . sum . map (\x -> x * x) . toList

normalize :: Quadruple -> Quadruple
normalize q@(Quadruple x' y' z' w') = 
  let mag = magnitude q
  in Quadruple (x' / mag) (y' / mag) (z' / mag) (w' / mag)

dot :: Quadruple -> Quadruple -> Double
dot q1 q2 = sum $ zipWith (*) (toList q1) (toList q2)

-- 3D cross-product, ignore w
cross :: Quadruple -> Quadruple -> Quadruple
(Quadruple x1 y1 z1 _) `cross` (Quadruple x2 y2 z2 _) =
  vector (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)
 