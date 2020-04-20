module Logic.Network where

import Data.List
import qualified Debug.Trace as DBG
import Types.Network
import Logic.Maths

-- Just a pretty name for a pair of (neuron's activation, neuron's weighted input)
data Activation = Activation {activation :: Double,
                              weightedInput :: Double}

  deriving (Show, Eq) -- TODO: Why does removing deriving show here fail silently?

weightInputs :: [Activation] -> Neuron -> Double
weightInputs acts n = (bias n + ) . sum . zipWith (\a i -> activation a * i) acts $ ws
  where ws = inWeights n

-- |Given a list of inputs (of same size as Neuron's inWeights), produces the neurons activation + weighted input.
feedNeuron :: [Activation] -> Neuron -> Activation
feedNeuron inp n = 
  let weightedInput = weightInputs inp n
      activation    = sigmoid weightedInput in
        Activation activation weightedInput

-- |Given a list of activation (which will in practice be the previous layer's activations), produces activation for a list of neurons.
feedLayer :: [Activation] -> [Neuron] -> [Activation]
feedLayer inp = fmap (feedNeuron inp)

-- |Given a list of inputs, feeds them through a sequence of layers, returning each layer's activation
feedBrain :: [Activation] -> [[Neuron]] -> [[Activation]]
feedBrain = scanl' feedLayer

hadamard :: [Double] -> [Double] -> [Double]
a `hadamard` b = zipWith (*) a b

-- Error for the output layer: Needs ys
equation1 :: [Activation] -> [Double] -> [Double]
equation1 acts = zipWith quadratic' (activation <$> acts)

-- Error for a layer L given error for L+1: Needs outgoing weights from 
equation2 :: [Double] -> [Neuron] -> [Activation] -> [Double]
equation2 errorsPlus1 neuronsPlus1 acts =
  let ithForEachNeuron ns i = map ((!! i) . inWeights) ns
      toMult                = map (ithForEachNeuron neuronsPlus1) [0..length neuronsPlus1 - 1] 
      firstThing            = map (sum . zipWith (*) errorsPlus1) toMult -- this and the above are to do a weight matrix multiplication
      secondThing           = sigmoid' . weightedInput <$> acts in
  firstThing `hadamard` secondThing

-- Rate of change for bias in layer L; needs error for layer L
equation3 :: [Double] -> [Double]
equation3 errors = errors

-- Rate of change for weights in layer L; needs error for layer L+1 and acts for layer L
equation4 :: [Double] -> [Activation] -> [[Double]]
equation4 errorsPlus1 acts = 
  let folder acc (Activation a _) = map (* a) errorsPlus1 : acc
    in reverse $ foldl' folder [] acts

-- For tests: 
-- weightInputs [Activation 1.0 _, Activation 2.0 _] Neuron { bias = 0.0, inWeights = [2.0, 3.0] } ==> [2.0,6.0]

-- feedNeuron [Activation 1.0 _, Activation 2.0 _] Neuron { bias = 1.0, inWeights = [0.001, 0.001] } ==> Activation 0.7316480054113164 1.003