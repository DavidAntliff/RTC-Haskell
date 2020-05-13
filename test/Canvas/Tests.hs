module Canvas.Tests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Canvas
import Color
import Math (almostEqual)
import Color.Tests (colorAlmostEqual)

unitTests :: TestTree

unitTests = testGroup "Canvas Unit Tests"
  [ testGroup "Creating a canvas" $
      let c = canvas 10 20
      in [ testCase "width" $ assertEqual [] (width c) 10
         , testCase "height" $ assertEqual [] (height c) 20
         , testCase "all black" $ assertBool [] $ and $ [pixelAt (x, y) c == Color 0 0 0 | x <- [0..9], y <- [0..19]]
         ]
  , testGroup "Writing pixels to a canvas" $
      let red = Color 1 0 0
          c = writePixelAt (2, 3) red $ canvas 10 20
      in [ testCase "red pixel at 2 3" $ assertBool [] $ colorAlmostEqual red (pixelAt (2, 3) c)
         , testCase "rest black" $ assertBool [] $ and $ [pixelAt (x, y) c == Color 0 0 0 | x <- [0..9], x /= 2, y <- [0..19], y /= 3]
      ]
  , testCase "Set all pixels to one color" $
      let c1 = Color 0.5 0.6 0.7
          c = setAllPixelsTo c1 $ canvas 10 20
      in assertBool [] $ and $ [pixelAt (x, y) c == c1 | x <- [0..9], y <- [0..19]]
  , testCase "Constructing the PPM header" $
      let c = canvas 5 3
          ppm = ppmFromCanvas c
      in assertEqual [] "P3\n5 3\n255\n" (unlines $ take 3 $ lines ppm)
  , testCase "Constructing the PPM header" $
      let c = canvas 4 9
          ppm = ppmFromCanvas c
      in assertEqual [] "P3\n4 9\n255\n" (unlines $ take 3 $ lines ppm)
  , testCase "Constructing the PPM pixel data" $
      let c1 = Color 1.5 0 0
          c2 = Color 0 0.5 0
          c3 = Color (-0.5) 0 1
          c = writePixelAt (4, 2) c3 $ writePixelAt (2, 1) c2 $ writePixelAt (0, 0) c1 $ canvas 5 3
          ppm = ppmFromCanvas c
      in assertEqual [] "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0\n\
                        \0 0 0 0 0 0 0 128 0 0 0 0 0 0 0\n\
                        \0 0 0 0 0 0 0 0 0 0 0 0 0 0 255\n" (unlines $ take 3 $ drop 3 $ lines ppm)                     
  , testCase "Splitting long lines in PPM files" $
      let c1 = Color 1 0.8 0.6
          c = setAllPixelsTo c1 $ canvas 10 2
          ppm = ppmFromCanvas c
      in assertBool [] $ all ((<= 70) . length) (lines ppm)                                                         
  ]


