module Lib
    ( someFunc
    , checkAssert
    ) where

import Control.Exception

someFunc :: String
someFunc = "someFunc"

checkAssert :: Bool
checkAssert = assert (1 == 2) False
