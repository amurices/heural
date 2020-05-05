module Lib
    ( someFunc
    ) where

import IO.Network
import System.Random

someFunc :: IO ()
someFunc = makeBrain randomIO (Right randomIO) [3,3,3] >>= print
