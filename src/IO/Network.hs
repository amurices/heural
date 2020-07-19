module IO.Network where 

import System.Random
import Control.Monad
import Types.Network (Neuron(..))
import Logic.Network
import Util


-- singleNeuronWeightsAndBias :: Int -> IO Double -> IO Neuron
-- singleNeuronWeightsAndBias prevSize generator = 
--     Neuron <$> generator <*> replicateM prevSize generator

-- layerWeightsAndBias :: IO Double -> Int -> Int -> IO [Neuron]
-- layerWeightsAndBias generator prevSize curSize  =
--     replicateM curSize (singleNeuronWeightsAndBias prevSize generator)

-- makeLayers :: IO Double -> [Int] -> IO [[Neuron]]
-- makeLayers generator sizes = tail <$> zipWithM (layerWeightsAndBias generator) (1:sizes) sizes 

getLayerSizes :: IO [Int]
getLayerSizes = map read . words <$> getLine

-- Generates a layer of ncur neurons, with each having nprev random input Doubles
makeLayer :: IO Double -> Int -> Int -> IO [Neuron]
makeLayer gen nprev ncur = replicateM ncur biasedNeuron
  where biasedNeuron = Neuron <$> gen <*> replicateM nprev gen

makeLayers :: IO Double -> [Int] -> IO [[Neuron]]
makeLayers gen ints = tail <$> zipWithM (makeLayer (gauss 0.01 gen)) (1:ints) ints
