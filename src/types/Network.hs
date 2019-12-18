module Types.Network where

import Text.Printf

type Bias = Float
type Weight = Float

printWeights :: [Weight] -> String
printWeights ws = concatMap (\weight -> printf "%.3f " weight) ws

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
toLayer x = Layer x

newtype Brain = Brain [Layer]
instance Show Brain where
  show (Brain x) = concat $ zipWith printCounterAndLayer [1..] x
    where printCounterAndLayer c layer = printf "Layer %d: \n" (c :: Int) ++ show layer ++ "XXXX\nXXXX\n"

newtype Output = Output [[(Float, [Weight])]]
instance Show Output where
  show (Output x) = concatMap (\layerOutput ->
    concatMap (\neuronOutput ->
      printf "Activ: %.4f" (fst neuronOutput) ++ ", " ++ 
      "ws: " ++ printWeights (snd neuronOutput) ++ "\n") layerOutput ++ "\n") x