{-# LANGUAGE BangPatterns #-}

module IO.Network where 

import Control.Monad
import Types.Network (Neuron(..))
import Util

-- |Generates a layer of currLayer neurons, with each having prevLayer random input Doubles
makeLayer :: (Monad m) => m Double -> Int -> Int -> m [Neuron]
makeLayer gen prevLayer currLayer = replicateM currLayer gennedNeuron
  where gennedNeuron = Neuron <$> gen <*> replicateM prevLayer gen

makeLayers :: (Monad m) => m Double -> [Int] -> m [[Neuron]]
makeLayers gen ints =
  let !res = nuTap . tail <$> zipWithM (makeLayer (gauss 0.01 gen)) (1:ints) (nuTap ints)
  in
    res
