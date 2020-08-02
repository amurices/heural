{-# LANGUAGE BangPatterns #-}

module IO.Network where 

import Control.Monad
import Types.Network (Neuron(..), Network(..))
import Util

-- |Generates a layer of currLayer neurons, with each having prevLayer random input Doubles
makeLayer :: (Monad m) => m Double -> [y] -> [y] -> m [Neuron]
makeLayer gen prevLayer currLayer = mapM gennedNeuron currLayer
  where gen'         = \_ -> gen -- This is to mapM over the input traversable.
        gennedNeuron = \_ -> Neuron <$> gen' undefined <*> mapM gen' prevLayer

makeNetwork :: (Monad m) => Double -> 
                            (Double -> Double) -> 
                            (Double -> Double) -> 
                            m Double -> 
                            [Int] -> 
                            m Network
makeNetwork eta actFn actFn' gen ints = do
  randomNet <- tail <$> 
                 zipWithM (makeLayer (gauss 0.01 gen))
                          (map (flip replicate undefined) (1:ints))
                          (map (flip replicate undefined) ints)
  pure $ Network {eta = eta,
                  activationFunction = actFn,
                  activationFunction' = actFn',
                  net = randomNet}
