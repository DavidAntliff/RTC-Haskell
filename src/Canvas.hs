module Canvas ( Canvas
              , canvas
              , width
              , height
              , pixelAt
              , writePixelAt
              , setAllPixelsTo
              , ppmFromCanvas
              ) where

import Data.Array
import Data.List (elemIndex)

import Color
import Internal.Canvas

data Canvas = Canvas { width :: Int
                     , height :: Int
                     , pixels :: Array (Int, Int) Color
                     } deriving (Show)

canvas :: Int -> Int -> Canvas
canvas w h = Canvas w h $ array ((0, 0), (w - 1, h -1)) [ ((i, j), Color 0 0 0) | i <- [0..w-1], j <- [0..h-1]]

pixelAt :: (Int, Int) -> Canvas -> Color
pixelAt (x, y) c = pixels c ! (x, y)

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
