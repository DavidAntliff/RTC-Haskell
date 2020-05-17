-- stack bench --ba "--output bench.html"

import Criterion.Main
import Numeric.LinearAlgebra
import Linear.Matrix
import Linear.V4

-- the function to benchmark
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

hmatrixMult :: Double -> Matrix R
hmatrixMult m =
  let m1 = matrix 4 [m..(m + 15)] * ident 4
      m2 = (4><4) [m..] + row [1, 2, 3, 4]
  in m1 Numeric.LinearAlgebra.<> m2

linearMult :: Double -> V4 (V4 Double)
linearMult m =
  let m1 = V4 (V4 (m + 1) m m m) (V4 m (m + 6) m m) (V4 m m (m + 11) m) (V4 m m m (m + 16))
      m2 = (V4 (V4 (m + 1) (m + 3) (m + 5) (m + 7))
               (V4 (m + 5) (m + 7) (m + 9) (m + 11))
               (V4 (m + 9) (m + 11) (m + 13) (m + 15))
               (V4 (m + 13) (m + 15) (m + 17) (m + 19)))
  in m1 !*! m2

--data Row4 = Row4 [Double]
--data Matrix44 = Matrix44 [Row4]

matMult :: [[Double]] -> [[Double]] -> [[Double]]
matMult a b =
  let r0 = getRow a 0
      r1 = getRow a 1
      r2 = getRow a 2
      r3 = getRow a 3
      c0 = getColumn b 0
      c1 = getColumn b 1
      c2 = getColumn b 2
      c3 = getColumn b 3
  in [ [dotProduct r0 c0, dotProduct r0 c1, dotProduct r0 c2, dotProduct r0 c3]
     , [dotProduct r1 c0, dotProduct r1 c1, dotProduct r1 c2, dotProduct r1 c3]
     , [dotProduct r2 c0, dotProduct r2 c1, dotProduct r2 c2, dotProduct r2 c3]
     , [dotProduct r3 c0, dotProduct r3 c1, dotProduct r3 c2, dotProduct r3 c3]
     ]

getRow :: [[Double]] -> Int -> [Double]
getRow m i = m !! i

getColumn :: [[Double]] -> Int -> [Double]
getColumn m i = [ getRow m 0 !! i
                , getRow m 1 !! i
                , getRow m 2 !! i
                , getRow m 3 !! i
                ]

dotProduct :: [Double] -> [Double] -> Double
dotProduct row col = sum $ zipWith (*) row col

-- Matrix44 [(Row4 [1, 2, 3, 4]), (Row4 [1,2,3,4]), (Row4 [1,2,3,5]), (Row4 [1,2,3,6])]

naiveMult :: Double -> [[Double]]
naiveMult m =
  let m1 = [ [m + 1, m, m, m]
           , [m, m + 6, m, m]
           , [m, m, m + 11, m]
           , [m, m, m, m + 16]
           ]
      m2 = [ [m + 1, m + 3, m + 5, m + 7]
           , [m + 5, m + 7, m + 9, m + 11]
           , [m + 9, m + 11, m + 13, m + 15]
           , [m + 13, m + 15, m + 17, m + 19]
           ]
  in m1 `matMult` m2

-- Our benchmark harness.
main :: IO()
main = defaultMain [ bgroup "hmatrix" [ bench "mult 1" $ nf hmatrixMult 1.0
                                      ]
                   , bgroup "linear" [ bench "mult 1" $ nf linearMult 1.0
                                     ]
                   , bgroup "naive" [ bench "mult 1" $ nf naiveMult 1.0
                                     ]
                   ]
