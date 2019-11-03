module Main where

import IO.Network

main = makeBrain [3,3,3] >>= print
