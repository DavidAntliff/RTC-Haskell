module Canvas.Tests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Canvas
import Math (almostEqual)

unitTests :: TestTree

colorAlmostEqual :: Color -> Color -> Bool
colorAlmostEqual c1 c2 = and $ zipWith almostEqual (toList c1) (toList c2)  

unitTests = testGroup "features/tuples.feature Chapter 2"
  [ testGroup "Colors are (red, green, blue) tuples" $
      let c = Color (-0.5) 0.4 1.7 in
        [ testCase "red" $ assertEqual [] (red c) (-0.5)
        , testCase "green" $ assertEqual [] (green c) 0.4
        , testCase "blue" $ assertEqual [] (blue c) 1.7
        ]
  , testCase "Adding colors" $
      let c1 = Color 0.9 0.6 0.75
          c2 = Color 0.7 0.1 0.25
      in assertBool [] $ colorAlmostEqual (c1 |+| c2) (Color 1.6 0.7 1.0)
  , testCase "Subtracting colors" $
      let c1 = Color 0.9 0.6 0.75
          c2 = Color 0.7 0.1 0.25
      in assertBool [] $ colorAlmostEqual (c1 |-| c2) (Color 0.2 0.5 0.5)
  , testGroup "Multiplying a color" $
      let c = Color 0.2 0.3 0.4
      in [ testCase "by a scalar" $ assertBool [] $ colorAlmostEqual (c |* 2) (Color 0.4 0.6 0.8)
         , testCase "by a scalar prefix" $ assertBool [] $ colorAlmostEqual (2 *| c) (Color 0.4 0.6 0.8)
         ]  
  , testCase "Multiplying colors" $
      let c1 = Color 1 0.2 0.4
          c2 = Color 0.9 1 0.1
      in assertBool [] $ colorAlmostEqual (c1 |*| c2) (Color 0.9 0.2 0.04)

  , testGroup "Creating a canvas" $
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
