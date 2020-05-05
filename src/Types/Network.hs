module Types.Network where

data Neuron = Neuron {bias :: Double, inWeights :: [Double]}
  deriving (Eq, Show)

data Activation = Activation {activation :: Double,
                              weightedInput :: Double}
  deriving (Show, Eq)
