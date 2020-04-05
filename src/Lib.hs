module Lib
    ( someFunc
    ) where

import IO.Network

someFunc :: IO ()
someFunc = makeBrain [3,3,3] >>= print
