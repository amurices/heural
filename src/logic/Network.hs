module Logic.Network where

import Data.List
import Types.Network
import Logic.Maths

feedNeuron :: [Float] -> Neuron -> Float
feedNeuron inp n = sigmoid . (b + ) . sum . zipWith (*) inp $ ws
  where ws = weights n
        b  = bias n

feedLayer :: [Float] -> Layer -> [Float]
feedLayer inp (Layer ns) = fmap (feedNeuron inp) ns

-- Given an input, returns activations of each layer except "0th", which is the input itself
feedBrain :: [Float] -> Brain -> [[Float]]
feedBrain inp (Brain ls) = tail $ scanl' feedLayer inp ls

-- This is dumb because it traverses the entire matrix twice
feedBrainWithWeightsEasy :: [Float] -> Brain -> [[(Float, [Weight])]]
feedBrainWithWeightsEasy inp b@(Brain ls) = zipWith layerActivationsAndWeights activations ls
  where activations = feedBrain inp b
        layerActivationsAndWeights las (Layer l) = zipWith (\a n -> (a, weights n)) las l

-- Given an input and a list of layers, returns activations of each layer of neuron
feedBrainWithWeights :: [Float] -> [Layer] -> x
feedBrainWithWeights inp layers = undefined
