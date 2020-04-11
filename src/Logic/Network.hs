module Logic.Network where

import Data.List
import Types.Network
import Logic.Maths

-- |Given a list of inputs (of same size as Neuron's inEdges), produces the neurons activation.
feedNeuron :: [Double] -> Neuron -> Double
feedNeuron inp n = sigmoid . (b + ) . sum . zipWith (*) inp $ ws
  where ws = inEdges n
        b  = bias n

-- |Given a list of inputs (which will in practice be the previous layer's activations), produces activations for a list of neurons.
feedLayer :: [Double] -> [Neuron] -> [Double]
feedLayer inp = fmap (feedNeuron inp)

-- |Given a list of inputs, feeds them through a sequence of layers, returning each layer's activation
feedBrain :: [Double] -> [[Neuron]] -> [[Double]]
feedBrain = scanl' feedLayer