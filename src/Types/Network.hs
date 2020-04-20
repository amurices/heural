module Types.Network where

import Text.Printf

type Bias = Double

data Neuron = Neuron {bias :: Bias, inWeights :: [Double]}
  deriving (Eq)

instance Show Neuron where
  show (Neuron b ws) = printf "b: %.1f " b ++ "i:" ++  concatMap ((" " ++) . printf "%.2f") ws ++ "\n"