module Types.Network where

import Data.Vector

data Neuron = Neuron {bias :: Double, inWeights :: [Double]}
  deriving (Eq, Show)

data Activation = Activation {activation :: Double,
                              weightedInput :: Double}
  deriving (Show, Eq)

data UV_Neuron = UV_Neuron {uv_bias :: Double, uv_inWeights :: Vector Double}
  deriving (Eq, Show)

data UV_Activation = UV_Activation {uv_activation :: Double, uv_weightedInput :: Double}
  deriving (Show, Eq)