module Logic.Network 
       (weightInputs,
        feedNeuron,
        feedNeuronV,
        feedLayer,
        feedBrain,
        feedBrainV,
        backpropagate,
        errorLast,
        learn,
        learnV)
       where

import Data.List
import Types.Network
import Logic.Maths
import qualified Data.Vector as V

weightInputs :: [Activation] -> Neuron -> Double
weightInputs acts n = (bias n + ) . sum . zipWith (\a i -> activation a * i) acts $ ws
  where ws = inWeights n

weightInputsV :: V.Vector UV_Activation -> UV_Neuron -> Double
weightInputsV as n = (uv_bias n + ) . sum . V.zipWith (\a i -> uv_activation a * i) as $ ws
  where ws = uv_inWeights n

-- |Given a list of inputs (of same size as Neuron's inWeights), produces the neurons activation + weighted input.
feedNeuron :: [Activation] -> Neuron -> Activation
feedNeuron inp n = 
  let weightedInput = weightInputs inp n
      activation    = activationFn weightedInput in
        Activation activation weightedInput

-- |VECTOR VERSION: Given a list of inputs (of same size as Neuron's inWeights), produces the neurons activation + weighted input.
feedNeuronV :: V.Vector UV_Activation -> UV_Neuron -> UV_Activation
feedNeuronV inp n = 
  let weightedInput = weightInputsV inp n
      activation    = activationFn weightedInput in
        UV_Activation activation weightedInput

-- |Given a list of activation (which will in practice be the previous layer's activations), produces activation for a list of neurons.
feedLayer :: [Activation] -> [Neuron] -> [Activation]
feedLayer inp = fmap (feedNeuron inp)

-- |VECTOR VERSION: Given a list of activation (which will in practice be the previous layer's activations), produces activation for a list of neurons.
feedLayerV :: V.Vector UV_Activation -> V.Vector UV_Neuron -> V.Vector UV_Activation
feedLayerV inp = fmap (feedNeuronV inp)

-- |Given a list of inputs, feeds them through a sequence of layers, returning each layer's activation
feedBrain :: [Double] -> [[Neuron]] -> [[Activation]]
feedBrain input = tail . scanl' feedLayer input'
  where input' = (\x -> Activation {weightedInput = 0.0, activation = x}) <$> input

-- |VECTOR VERSION: Given a list of inputs, feeds them through a sequence of layers, returning each layer's activation
feedBrainV :: V.Vector Double -> V.Vector (V.Vector UV_Neuron) -> V.Vector (V.Vector UV_Activation)
feedBrainV input = V.tail . V.scanl' feedLayerV input'
  where input' = UV_Activation 0.0 <$> input

hadamard :: [Double] -> [Double] -> [Double]
a `hadamard` b = zipWith (*) a b

hadamardV :: V.Vector Double -> V.Vector Double -> V.Vector Double
a `hadamardV` b = V.zipWith (*) a b

-- Error for the output layer: Needs ys
errorLast :: [Activation] -> [Double] -> [Double]
errorLast acts desired = zipWith (*) dCosted dActivated
  where dCosted = zipWith quadratic' (activation <$> acts) desired
        dActivated = map activationFn' (weightedInput <$> acts)

-- |VECTOR VERSION: Error for the output layer: Needs ys
errorLastV :: V.Vector UV_Activation -> V.Vector Double -> V.Vector Double
errorLastV acts desired = V.zipWith (*) dCosted dActivated
  where dCosted = V.zipWith quadratic' (uv_activation <$> acts) desired
        dActivated = fmap activationFn' (uv_weightedInput <$> acts)

-- Error for a layer L given error for L+1
errorL :: [Double] -> [Neuron] -> [Activation] -> [Double]
errorL errorsPlus1 neuronsPlus1 acts =
  let ithForEachNeuron ns i = map ((!! i) . inWeights) ns
      toMult                = map (ithForEachNeuron neuronsPlus1) [0..length acts - 1] 
      firstThing            = map (sum . zipWith (*) errorsPlus1) toMult -- this and the above are to do a weight matrix multiplication
      secondThing           = activationFn' . weightedInput <$> acts in
  firstThing `hadamard` secondThing

-- |VECTOR VERSION: Error for a layer L given error for L+1
errorLV :: V.Vector Double -> V.Vector UV_Neuron -> V.Vector UV_Activation -> V.Vector Double
errorLV errorsPlus1 neuronsPlus1 acts =
  let ithForEachNeuron ns i = V.map ((V.! i) . uv_inWeights) ns
      toMult                = V.map (ithForEachNeuron neuronsPlus1) (V.enumFromTo 0 (length acts - 1))
      firstThing            = V.map (V.sum . V.zipWith (*) errorsPlus1) toMult -- this and the above are to do a weight matrix multiplication
      secondThing           = activationFn' . uv_weightedInput <$> acts in
  firstThing `hadamardV` secondThing

-- given initial error, activations and a brain, return errors for all neurons
backpropagate :: [Double] -> [[Activation]] -> [[Neuron]] -> [[Double]]
backpropagate outputError activations brain = let
  staggeredActs   = tail $ reverse activations
  actsAndBrainRev = zip staggeredActs (reverse brain)
  in  -- this has to be a "staggered" fold; errorL operates on acts of current layer, but neurons and errors of next layer
    foldl' (\acc (act, n) -> errorL (head acc) n act : acc) [outputError] actsAndBrainRev

-- |VECTOR VERSION: given initial error, activations and a brain, return errors for all neurons
backpropagateV :: V.Vector Double -> V.Vector (V.Vector UV_Activation) -> V.Vector (V.Vector UV_Neuron) -> V.Vector (V.Vector Double)
backpropagateV outputError activations brain = let
  staggeredActs   = V.tail $ V.reverse activations
  actsAndBrainRev = V.zip staggeredActs (V.reverse brain)
  in  -- this has to be a "staggered" fold; errorL operates on acts of current layer, but neurons and errors of next layer
    V.foldl' (\acc (act, n) -> errorLV (V.head acc) n act `V.cons` acc) (V.singleton outputError) actsAndBrainRev


-- For an  eta, Given a neuron's error and activation, return a learned version of it
learnNeuron :: Double -> Neuron -> Double -> Activation -> Neuron
learnNeuron eta (Neuron b iw) e act = Neuron b' iw'
  where b'  =  b - eta * e
        iw' = map (* (1 - eta * e)) iw

-- For an  eta, Given a neuron's error and activation, return a learned version of it
learnNeuronV :: Double -> UV_Neuron -> Double -> UV_Activation -> UV_Neuron
learnNeuronV eta (UV_Neuron b iw) e act = UV_Neuron b' iw'
  where b'  = b - eta * e
        iw' = fmap (* (1 - eta * e)) iw

learn :: [[Neuron]] -> [Double] -> [Double] -> [[Neuron]]
learn brain input desired = let
  allActivations = feedBrain input brain
  outputError    = errorLast (last allActivations) desired 
  allErrors      = backpropagate outputError allActivations brain
  in
    zipWith3 (zipWith3 (learnNeuron 0.002)) brain allErrors allActivations

learnV :: V.Vector (V.Vector UV_Neuron) -> V.Vector Double -> V.Vector Double -> V.Vector (V.Vector UV_Neuron)
learnV brain input desired = let
  allActivations = feedBrainV input brain
  outputError    = errorLastV (V.last allActivations) desired 
  allErrors      = backpropagateV outputError allActivations brain
  in
    V.zipWith3 (V.zipWith3 (learnNeuronV 0.002)) brain allErrors allActivations

-- For tests: 
-- weightInputs [Activation 1.0 _, Activation 2.0 _] Neuron { bias = 0.0, inWeights = [2.0, 3.0] } ==> [2.0,6.0]

-- feedNeuron [Activation 1.0 _, Activation 2.0 _] Neuron { bias = 1.0, inWeights = [0.001, 0.001] } ==> Activation 0.7316480054113164 1.003

-- last $ feedBrain [Activation 1 1, Activation 1 1, Activation 1 1] ourBrain ==> 
-- with relu and relu': [Activation {activation = 21.6, weightedInput = 21.6},Activation {activation = 26.8, weightedInput = 26.8}] 
-- with sigmoid and sigmoid': [Activation {activation = 0.998969150393322, weightedInput = 6.8763405746014925},Activation {activation = 0.9997724097287007, weightedInput = 8.387735985182916}]

-- learn2 ourBrain [1,1,1] [1.0001, 2]  ==>
-- with relu and relu' [[Neuron {bias = 0.85304028, inWeights = [0.170608056,0.255912084,0.341216112]},Neuron {bias = 0.8439603, inWeights = [0.42198015,0.50637618,0.5907722099999999]},Neuron {bias = 0.83488032, inWeights = [0.667904256,0.751392288,0.83488032]},Neuron {bias = 0.82580034, inWeights = [0.908380374,0.990960408,1.073540442]}],[Neuron {bias = 0.9588002, inWeights = [1.3423202799999998,1.4382003,1.5340803200000002,1.62996034]},Neuron {bias = 0.9504, inWeights = [1.71072,1.8057599999999998,1.9008,1.99584]}]]