{-# LANGUAGE BangPatterns #-}

module IO.Network where 

import Control.Monad
import Types.Network (Neuron(..), Network(..), NeuronP(..))
import Util

-- |Generates a layer of currLayer neurons, with each having prevLayer random input Doubles
makeLayer :: (Monad m) => m Double -> [y] -> [y] -> m [Neuron]
makeLayer gen prevLayer currLayer = mapM gennedNeuron currLayer
  where gen'         = \_ -> gen -- This is to mapM over the input traversable.
        gennedNeuron = \_ -> Neuron <$> gen' undefined <*> mapM gen' prevLayer

makeLayers :: (Monad m) => m Double -> [Int] -> m [[Neuron]]
makeLayers gen ints = tail <$> 
  zipWithM (makeLayer (gauss 0.01 gen))
           (map (flip replicate undefined) (1:ints))
           (map (flip replicate undefined) ints)

-- |Generates a layer of currLayer neurons, with each having prevLayer random input Doubles
makeLayerP :: (Monad m, Traversable t) => m Double -> t y -> t y -> m (t (NeuronP t))
makeLayerP gen prevLayer currLayer = mapM gennedNeuron currLayer
  where gen'         = \_ -> gen -- This is to mapM over the input traversable.
        gennedNeuron = \_ -> NeuronP <$> gen' undefined <*> mapM gen' prevLayer
