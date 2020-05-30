-- Chapter 4 Exercise - Clock face

module Main where

import Data.List (foldl')

import Quadruple
import Canvas
import Color (Color(..))
import Transformations
import Matrix

main :: IO ()
main = do
  let c = Canvas.canvas 400 400
  let c' = drawClockFace c
  let ppm = Canvas.ppmFromCanvas c'
  writeFile "output.ppm" ppm

writePixelAtBoundsCheck :: (Int, Int) -> Color -> Canvas -> Canvas
writePixelAtBoundsCheck (x, y) v c
    | (x >= 0) && (x < width c) && (y >= 0) && (y < height c) = writePixelAt (x, y) v c
    | otherwise = c  -- drop the bad coordinate

-- rotate around z axis
drawClockFace :: Canvas -> Canvas
drawClockFace c =
  let hours = [0..11]
      points = map getHourVector hours
      h = fromIntegral $ height c :: Double
      w = fromIntegral $ width c :: Double
      points' = scalePoints (0.9 / 2.0 * min h w) points
      points'' = movePoints (w / 2.0) (h / 2.0) points'
      c' = drawPoints c points''
  in c'
--      p = point 0 1 0  -- 12 o'clock
--      r = rotationZ $ 2 * pi / 12 -- per hour angle
--      p' = r |*| p
      
getHourVector :: Int -> Quadruple
getHourVector h = 
  let r = rotationZ $ (fromIntegral h :: Double) * 2 * pi / 12
      p = point 0 1 0 -- 12 o'clock
  in r |*| p

scalePoints :: Double -> [Quadruple] -> [Quadruple]
scalePoints s points =
  let transform = scaling s s 0
  in map (transform |*|) points  

movePoints :: Double -> Double -> [Quadruple] -> [Quadruple]
movePoints x y points =
  let transform = translation x y 0
  in map (transform |*|) points

drawPoints :: Canvas.Canvas -> [Quadruple] -> Canvas.Canvas
drawPoints c points = 
  let coords = [ (round $ x q, Canvas.height c - round (y q)) | q <- points ]
      c' = foldl' (\ca p -> writePixelAtBoundsCheck p (Color 1 1 1) ca) c coords
  in c'