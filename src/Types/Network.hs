module Types.Network where

import Text.Printf

type Bias = Double

data Neuron = Neuron {bias :: Bias, inEdges :: [Double]}
  deriving (Show, Eq)

