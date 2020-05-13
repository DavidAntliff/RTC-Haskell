module Internal.Canvas ( getRow
                       , ppmFromFloat
                       , ppmFromColor
                       , splitLineBy
                       , splitLinesBy
                       ) where

import Data.Array
import Data.List (elemIndex)

import Color

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
