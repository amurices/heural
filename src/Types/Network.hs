{-# LANGUAGE FlexibleContexts, UndecidableInstances, StandaloneDeriving #-}

module Types.Network (
  Neuron(..), 
  Activation(..),
  Network,
  NeuronP(..),
  ) where

data Neuron = Neuron {bias :: Double, inWeights :: [Double]}
  deriving (Eq, Show)

data Activation = Activation {activation :: Double,
                              weightedInput :: Double}
  deriving (Show, Eq)

data NeuronP t = NeuronP {biasP :: Double, inWeightsP :: t Double}
deriving instance Eq (t Double) => Eq (NeuronP t)
deriving instance Show (t Double) => Show (NeuronP t)

data Network t = Network {activationFn :: (Double -> Double),
                          net :: t (t (NeuronP t))}
