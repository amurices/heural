module Lib
    ( someFunc
    ) where

import IO.Network
import qualified Logic.Network as LN
import System.Random
import System.Environment

someFunc :: IO ()
someFunc = do 
    putStrLn "Input the size of each layer of the brain"
    sizes <- map read . words <$> getLine
    putStrLn "Input the iteration number"
    its <- read <$> getLine
    brain <- makeBrain randomIO (Right randomIO) sizes
    let input      = replicate (head sizes) 1
        desiredOut = replicate (last sizes) 6.9420
    evolvedBrain <- go input desiredOut its brain
    print (last $ LN.feedBrain input evolvedBrain)
    where go _ _ 0 evolvedBrain = pure evolvedBrain
          go input desiredOut its currBrain = go input desiredOut (its - 1) (LN.learn currBrain input desiredOut)

