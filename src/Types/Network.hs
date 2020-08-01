{-# LANGUAGE FlexibleContexts, UndecidableInstances, StandaloneDeriving #-}

module Types.Network (
  Neuron(..), 
  Activation(..),
  Network(..),
  ) where

data Neuron = Neuron {bias :: Double, inWeights :: [Double]}
  deriving (Eq, Show)

data Activation = Activation {activation :: Double,
                              weightedInput :: Double}
  deriving (Show, Eq)

data Network = Network {activationFunction :: (Double -> Double),
                        activationFunction' :: (Double -> Double),
                        eta :: Double,
                        net :: [[Neuron]]}

{- TODO: For when generalizing this to any traversable. P for Parametrized.
   Then the layer-fn below should work for generating this type.

data NeuronP t = NeuronP {biasP :: Double, inWeightsP :: t Double}
deriving instance Eq (t Double) => Eq (NeuronP t)
deriving instance Show (t Double) => Show (NeuronP t)

data NetworkP t = NetworkP {activationFnP :: (Double -> Double),
                            activationFnP' :: (Double -> Double),
                            netP :: t (t (NeuronP t))}

makeLayerP :: (Monad m, Traversable t) => m Double -> t y -> t y -> m (t (NeuronP t))
makeLayerP gen prevLayer currLayer = mapM gennedNeuron currLayer
  where gen'         = \_ -> gen -- This is to mapM over the input traversable.
        gennedNeuron = \_ -> NeuronP <$> gen' undefined <*> mapM gen' prevLayer
-}