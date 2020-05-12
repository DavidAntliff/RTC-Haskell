module Canvas ( Color(..)
              , toList
              , (|+|)
              , (|-|)
              , (|*)
              , (*|)
              , (|*|)
              , Canvas
              , canvas
              , width
              , height
              , pixelAt
              , writePixelAt
              , setAllPixelsTo
              , ppmFromCanvas
              -- For testing only:
              , getRow
              , ppmFromFloat
              , ppmFromColor
              , splitLineBy
              , splitLinesBy
              ) where

import Data.Array
import Data.List (elemIndex)

data Color = Color { red, green, blue :: Float } deriving (Eq, Show)

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

data Canvas = Canvas { width :: Int
                     , height :: Int
                     , pixels :: Array (Int, Int) Color
                     } deriving (Show)

canvas :: Int -> Int -> Canvas
canvas w h = Canvas w h $ array ((0, 0), (w - 1, h -1)) [ ((i, j), Color 0 0 0) | i <- [0..w-1], j <- [0..h-1]]

pixelAt :: (Int, Int) -> Canvas -> Color
pixelAt (x, y) c = (pixels c) ! (x, y)

writePixelAt :: (Int, Int) -> Color -> Canvas -> Canvas
writePixelAt (x, y) v (Canvas w h p) = Canvas w h $ p // [((x, y), v)]

-- TODO: use STUArray for better performance

setAllPixelsTo :: Color -> Canvas -> Canvas
setAllPixelsTo color (Canvas w h _) = Canvas w h $ array ((0, 0), (w - 1, h -1)) [ ((i, j), color) | i <- [0..w-1], j <- [0..h-1]]

ppmFromCanvas :: Canvas -> String
ppmFromCanvas c =
  let w = show $ width c
      h = show $ height c
      depth = 255
      pixels' = pixels c
      headerLines = ["P3", w ++ " " ++ h, show depth]
      rowLines = [getRow ri depth pixels' | ri <- [0..(height c)-1]]
      splitLines = splitLinesBy 70 rowLines
  in unlines $ headerLines ++ splitLines

getRow :: Int -> Int -> Array (Int, Int) Color -> String
getRow ri depth a = unwords $ [ppmFromColor depth (a ! (ci, ri)) | ci <- [0 .. (fst $ snd $ bounds a)]]

-- Convert floating color component to integer value, clamped between 0 and depth inclusive
ppmFromFloat :: Int -> Float -> Int
ppmFromFloat depth = round . (fromIntegral depth *) . max 0.0 . min 1.0

ppmFromColor :: Int -> Color -> String
ppmFromColor depth (Color r g b) =
  let r' = ppmFromFloat depth r
      g' = ppmFromFloat depth g
      b' = ppmFromFloat depth b
  in unwords $ map show [r', g', b']

-- Split lines exceeding limit by inserting newline at position of a space
splitLinesBy :: Int -> [String] -> [String]
splitLinesBy limit = concatMap (splitLineBy limit)

-- find the first space at or to the left of the limit
splitLineBy :: Int -> String -> [String]
splitLineBy limit line
    | length line < limit = [line]
    | otherwise =
        let (a, _) = splitAt (limit + 1) line
            a' = reverse a
            idx = case elemIndex ' ' a' of
                Nothing -> 0
                Just val -> limit - val
        in take idx line : splitLineBy limit (drop (idx + 1) line) 
