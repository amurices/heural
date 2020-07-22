module Logic.Network 
       (weightInputs,
        feedNeuron,
        feedLayer,
        feedBrain,
        backpropagate,
        errorLast,
        learn,
        learnMany)
       where

import Data.List
import Types.Network
import Logic.Maths

weightInputs :: [Activation] -> Neuron -> Double
weightInputs acts n = (bias n + ) . sum . zipWith (\a i -> activation a * i) acts $ ws
  where ws = inWeights n

-- |Given a list of inputs (of same size as Neuron's inWeights), produces the neurons activation + weighted input.
feedNeuron :: [Activation] -> Neuron -> Activation
feedNeuron inp n = 
  let weightedInput = weightInputs inp n
      activation    = activationFn weightedInput in
        Activation activation weightedInput

-- |Given a list of activation (which will in practice be the previous layer's activations), produces activation for a list of neurons.
feedLayer :: [Activation] -> [Neuron] -> [Activation]
feedLayer inp = fmap (feedNeuron inp)

-- |Given a list of inputs, feeds them through a sequence of layers, returning each layer's activation
feedBrain :: [Double] -> [[Neuron]] -> [[Activation]]
feedBrain input = tail . scanl' feedLayer input'
  where input' = (\x -> Activation {weightedInput = 0.0, activation = x}) <$> input

hadamard :: [Double] -> [Double] -> [Double]
a `hadamard` b = zipWith (*) a b

-- Error for the output layer: Needs ys
errorLast :: [Activation] -> [Double] -> [Double]
errorLast acts desired = zipWith (*) dCosted dActivated
  where dCosted = zipWith quadratic' (activation <$> acts) desired
        dActivated = map activationFn' (weightedInput <$> acts)

-- Error for a layer L given error for L+1
errorL :: [Double] -> [Neuron] -> [Activation] -> [Double]
errorL errorsPlus1 neuronsPlus1 acts =
  let ithForEachNeuron ns i = map ((!! i) . inWeights) ns
      toMult                = map (ithForEachNeuron neuronsPlus1) [0..length acts - 1] 
      firstThing            = map (sum . zipWith (*) errorsPlus1) toMult -- this and the above are to do a weight matrix multiplication
      secondThing           = activationFn' . weightedInput <$> acts in
  firstThing `hadamard` secondThing

-- |Given output error, activations and a brain, return errors for all neurons
backpropagate :: [Double] -> [[Activation]] -> [[Neuron]] -> [[Double]]
backpropagate outputError activations brain = let
  staggeredActs   = tail $ reverse activations
  actsAndBrainRev = zip staggeredActs (reverse brain)
  in  -- this has to be a "staggered" fold; errorL operates on acts of current layer, but neurons and errors of next layer
    foldl' (\acc (act, n) -> errorL (head acc) n act : acc) [outputError] actsAndBrainRev

-- |For an  eta, Given a neuron's error and previous layer's activations, return a learned version of it
learnNeuron :: Double -> [Activation] -> Neuron -> Double -> Neuron
learnNeuron eta acts (Neuron b iw) e = Neuron b' iw'
  where b'  = b - eta * e
        iw' = zipWith (\x y -> x - (eta * e * activation y)) iw acts

learn :: [[Neuron]] -> [Double] -> [Double] -> [[Neuron]]
learn brain input desired = let
  allActivations = feedBrain input brain
  outputError    = errorLast (last allActivations) desired 
  allErrors      = backpropagate outputError allActivations brain
  staggeredActs  = (flip Activation 0.0 <$> input) : allActivations
  in
    zipWith3 (\layer layerError prevLayerActs -> 
               zipWith (learnNeuron 0.002 prevLayerActs) layer layerError)
       brain allErrors staggeredActs

learnMany :: [[Neuron]] -> [[Double]] -> [[Double]] -> [[Neuron]]
learnMany brain inputs desired =
  let twoTogether = zip inputs desired in
    foldl' (\evolvingBrain (inp, des) -> learn evolvingBrain inp des) brain twoTogether

-- For tests: 
-- weightInputs [Activation 1.0 _, Activation 2.0 _] Neuron { bias = 0.0, inWeights = [2.0, 3.0] } ==> [2.0,6.0]

-- feedNeuron [Activation 1.0 _, Activation 2.0 _] Neuron { bias = 1.0, inWeights = [0.001, 0.001] } ==> Activation 0.7316480054113164 1.003

-- last $ feedBrain [Activation 1 1, Activation 1 1, Activation 1 1] ourBrain ==> 
-- with relu and relu': [Activation {activation = 21.6, weightedInput = 21.6},Activation {activation = 26.8, weightedInput = 26.8}] 
-- with sigmoid and sigmoid': [Activation {activation = 0.998969150393322, weightedInput = 6.8763405746014925},Activation {activation = 0.9997724097287007, weightedInput = 8.387735985182916}]

-- learn2 ourBrain [1,1,1] [1.0001, 2]  ==>
-- with relu and relu' [[Neuron {bias = 0.85304028, inWeights = [0.170608056,0.255912084,0.341216112]},Neuron {bias = 0.8439603, inWeights = [0.42198015,0.50637618,0.5907722099999999]},Neuron {bias = 0.83488032, inWeights = [0.667904256,0.751392288,0.83488032]},Neuron {bias = 0.82580034, inWeights = [0.908380374,0.990960408,1.073540442]}],[Neuron {bias = 0.9588002, inWeights = [1.3423202799999998,1.4382003,1.5340803200000002,1.62996034]},Neuron {bias = 0.9504, inWeights = [1.71072,1.8057599999999998,1.9008,1.99584]}]]