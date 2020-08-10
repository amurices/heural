{-# LANGUAGE FlexibleContexts, UndecidableInstances, StandaloneDeriving #-}

module Types.Network (
  Neuron(..), 
  Activation(..),
  Network(..)
  ) where

data Neuron = Neuron {bias :: Double, inWeights :: [Double]}
  deriving (Eq, Show)

data Activation = Activation {activation :: Double,
                              weightedInput :: Double}
  deriving (Show, Eq)

instance Num Activation where 
  (Activation a b) + (Activation c d) = Activation (a + c) (b + d)
  (Activation a b) * (Activation c d) = Activation (a * c) (b * d)
  abs (Activation a b) = (Activation (abs a) (abs b))
  signum a = undefined
  fromInteger i = Activation (fromInteger i) 0.0
  negate (Activation a b) = Activation (negate a) (negate b)

instance Fractional Activation where
  fromRational r = undefined
  (Activation a b) / (Activation c d) = Activation (a / c) (b / d)

data Network = Network {activationFunction :: (Double -> Double),
                        activationFunction' :: (Double -> Double),
                        eta :: Double,
                        net :: [[Neuron]]}
instance Show Network where
  show (Network _ _ _ network) = show network
instance Eq Network where
  (Network _ _ _ network1) == (Network _ _ _ network2) = network1 == network2
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