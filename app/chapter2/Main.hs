-- Chapter 1 Exercise - Cannon

module Main where

import Data.List (foldl')

import Quadruple
import Canvas
import Color (Color(..))

-- A projectile has a position (point) and a velocity (vector)
-- An environment has gravity (vector) and wind (vector)
data Projectile = Projectile { position :: Quadruple, velocity :: Quadruple } deriving (Show)
data Environment = Environment { gravity :: Quadruple, wind :: Quadruple } deriving (Show)

tick :: Environment -> Projectile -> Projectile
tick e p =
  let position' = position p |+| velocity p
      velocity' = velocity p |+| gravity e |+| wind e
  in Projectile position' velocity'

update :: Environment -> Projectile -> [Quadruple] -> [Quadruple]
update e p r
  | y (position p) < 0.0 = r
  | otherwise =
      let p' = tick e p
      in update e p' (r ++ [position p])

main :: IO ()
main = do
  -- gravity -0.1 unit/tick, and wind is -0.01 unit/tick.
  let environment = Environment (vector 0 (-0.1) 0) (vector (-0.01) 0 0)
  -- projectile starts one unit above the origin.
  -- velocity is normalized to 1 unit/tick.
  let projectile = Projectile (point 0 1 0) (normalize (vector 1 1.8 0) |* 11.25)
  let result = update environment projectile []
  
  let c = Canvas.canvas 900 550
  let c' = drawTrajectory c result
  let ppm = Canvas.ppmFromCanvas c'
  
  writeFile "output.ppm" ppm
  print ppm
 
  mapM_ print result
  print $ length result

drawTrajectory :: Canvas.Canvas -> [Quadruple] -> Canvas.Canvas
drawTrajectory c r = 
  let coords = [ (round $ x q, Canvas.height c - round (y q)) | q <- r ]
      c' = foldl' (\ca p -> Canvas.writePixelAt p (Color 1 0 0) ca) c coords
  in c'