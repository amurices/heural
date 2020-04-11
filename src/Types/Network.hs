module Types.Network where

import Text.Printf

type Bias = Double

data Neuron = Neuron {bias :: Bias, inEdges :: [Double]}
  deriving (Eq)

instance Show Neuron where
  show (Neuron b ws) = printf "b: %.3f " b ++ "ws: " ++  concatMap ((" " ++) . printf "%.2f") ws