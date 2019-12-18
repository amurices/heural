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
-- But it does work; it returns a list of activations + weights for each layer in reverse order
-- Steps 1 and 2
feedBrainWithWeightsEasy :: [Float] -> Brain -> [[(Float, [Weight])]]
feedBrainWithWeightsEasy inp b@(Brain ls) = reverse $ zipWith layerActivationsAndWeights activations ls
  where activations = feedBrain inp b
        layerActivationsAndWeights las (Layer l) = zipWith (\a n -> (a, weights n)) las l

-- Given a list of activations and a list of desired outputs, returns a list of deltas
-- Step 3
outputDelta :: [Float] -> [Float] -> [Float]
outputDelta activations ys = zipWith (*) dCostDA (map sigmoid' activations)
  where dCostDA = zipWith quadratic' activations ys

-- Given a delta0, a list of weights for each layer (so a list of lists) and a list of activations,
-- returns the errors for each layer. step 4
internalDeltas :: [Float] -> [[(Float, [Weight])]] -> [[Float]]
internalDeltas delta0 asws = undefined

-- Given an input and a list of layers, returns activations of each layer of neuron
feedBrainWithWeights :: [Float] -> [Layer] -> x
feedBrainWithWeights inp layers = undefined
