module IO.Network where 

import Control.Monad
import Types.Network (Neuron(..))
import Util

-- |Generates a layer of currLayer neurons, with each having prevLayer random input Doubles
makeLayer :: IO Double -> Int -> Int -> IO [Neuron]
makeLayer gen prevLayer currLayer = replicateM currLayer gennedNeuron
  where gennedNeuron = Neuron <$> gen <*> replicateM prevLayer gen

makeLayers :: IO Double -> [Int] -> IO [[Neuron]]
makeLayers gen ints = tail <$> zipWithM (makeLayer (gauss 0.01 gen)) (1:ints) ints
