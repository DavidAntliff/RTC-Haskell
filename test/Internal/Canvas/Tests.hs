module Internal.Canvas.Tests (unitTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Internal.Canvas
import Canvas

unitTests :: TestTree

unitTests = testGroup "Canvas Internal Unit Tests"
  [ testGroup "SplitLineBy"
      [ testCase "" $ assertEqual [] ["123 456"] (splitLineBy 8 "123 456")
      , testCase "" $ assertEqual [] ["123 456", "789 1011", "12"] (splitLineBy 8 "123 456 789 1011 12")
      , testCase "" $ assertEqual [] ["1 2", "3 4", "5 6", "7 8", "9 0"] (splitLineBy 4 "1 2 3 4 5 6 7 8 9 0")
      ]
  , testCase "SplitLinesBy" $ assertEqual [] [ "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
                                             , "153 255 204 153 255 204 153 255 204 153 255 204 153"
                                             , "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
                                             , "153 255 204 153 255 204 153 255 204 153 255 204 153"
                                             ]
                                             (splitLinesBy 70 [ "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153"
                                                              , "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 153"
                                                              ])
                                                              
  , testCase "PPM files are terminated by a newline character" $
      let c = canvas 5 3
          ppm = ppmFromCanvas c
      in assertBool [] $ last ppm == '\n'                                                            
  ]


