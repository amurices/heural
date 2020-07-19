module IO.Network where 

import System.Random
import Control.Monad
import Types.Network (Neuron(..))
import Logic.Network
import Util

-- Generates a layer of ncur neurons, with each having nprev random input Doubles
makeLayer :: IO Double -> IO Double -> Int -> Int -> IO [Neuron]
makeLayer gen bias nprev ncur = replicateM ncur biasedNeuron
  where biasedNeuron = Neuron <$> bias <*> replicateM nprev gen

makeBrain :: IO Double -> IO Double -> [Int] -> IO [[Neuron]]
makeBrain gen biasGen ints = tail <$> zipWithM (makeLayer (gauss 0.01 randomIO) (pure 0.0)) (1:ints) ints
