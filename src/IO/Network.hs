module IO.Network where 

import Control.Monad
import Types.Network (Neuron(..))
import Util

-- Generates a layer of ncur neurons, with each having nprev random input Doubles
makeLayer :: IO Double -> Int -> Int -> IO [Neuron]
makeLayer gen nprev ncur = replicateM ncur biasedNeuron
  where biasedNeuron = Neuron <$> gen <*> replicateM nprev gen

makeLayers :: IO Double -> [Int] -> IO [[Neuron]]
makeLayers gen ints = tail <$> zipWithM (makeLayer (gauss 0.01 gen)) (1:ints) ints
