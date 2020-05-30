module Transformations ( translation
                       , scaling
                       , rotationX
                       , rotationY
                       , rotationZ
                       , shearing
                       ) where

import Matrix

translation :: Double -> Double -> Double -> Matrix44
translation x y z = setElementAt (0, 3) x
                  $ setElementAt (1, 3) y
                  $ setElementAt (2, 3) z identity :: Matrix44

scaling :: Double -> Double -> Double -> Matrix44
scaling x y z = setElementAt (0, 0) x
              $ setElementAt (1, 1) y
              $ setElementAt (2, 2) z identity :: Matrix44

rotationX :: Double -> Matrix44
rotationX r = setElementAt (1, 1) (cos r)
            $ setElementAt (1, 2) (-sin r)
            $ setElementAt (2, 1) (sin r)
            $ setElementAt (2, 2) (cos r) identity :: Matrix44

rotationY :: Double -> Matrix44
rotationY r = setElementAt (0, 0) (cos r)
            $ setElementAt (0, 2) (sin r)
            $ setElementAt (2, 0) (-sin r)
            $ setElementAt (2, 2) (cos r) identity :: Matrix44

rotationZ :: Double -> Matrix44
rotationZ r = setElementAt (0, 0) (cos r)
            $ setElementAt (0, 1) (-sin r)
            $ setElementAt (1, 0) (sin r)
            $ setElementAt (1, 1) (cos r) identity :: Matrix44
            
shearing :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix44
shearing xy xz yx yz zx zy = setElementAt (0, 1) xy
                           $ setElementAt (0, 2) xz
                           $ setElementAt (1, 0) yx
                           $ setElementAt (1, 2) yz
                           $ setElementAt (2, 0) zx
                           $ setElementAt (2, 1) zy identity :: Matrix44