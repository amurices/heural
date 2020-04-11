module Types.Network where

import Text.Printf

type Bias = Double
type Weight = Double

printWeights :: [Weight] -> String
printWeights = concatMap (printf "%.3f ")

data Neuron = Neuron {bias :: Bias, weights :: [Weight]}
instance Show Neuron where
  show n =
    "b: " ++ show (bias n) ++ ", " ++ 
    "ws: " ++ printWeights (weights n)

newtype Layer = Layer [Neuron]
instance Show Layer where
  show (Layer x) = concatMap ((++ "\n") . show) x

fromLayer :: Layer -> [Neuron]
fromLayer (Layer x) = x

toLayer :: [Neuron] -> Layer
toLayer = Layer

newtype Brain = Brain [Layer]
instance Show Brain where
  show (Brain x) = concat $ zipWith printCounterAndLayer [1..] x
    where printCounterAndLayer c layer = printf "Layer %d: \n" (c :: Int) ++ show layer ++ "XXXX\nXXXX\n"

newtype Output = Output [[(Double, [Weight])]]
instance Show Output where
  show (Output x) = concatMap (\layerOutput ->
    concatMap (\neuronOutput ->
      printf "Activ: %.4f" (fst neuronOutput) ++ ", " ++ 
      "ws: " ++ printWeights (snd neuronOutput) ++ "\n") layerOutput ++ "\n") x