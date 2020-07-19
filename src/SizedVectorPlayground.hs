{-# LANGUAGE DataKinds, TypeOperators #-}

module SizedVectorPlayground where

import qualified Data.Vector.Sized as S
import qualified Data.Vector as Unsized
import GHC.TypeLits
import Control.Monad
import Util
import System.Random

data VNeuron n = Neuron {bias :: Double, inWeights :: S.Vector n Double}
  deriving (Eq, Show)

data VActivation n = Activation {activation :: Double, weightedInput :: S.Vector n Double}
  deriving (Show, Eq)

fazAlgoAi :: S.Vector n Double -> S.Vector (1 + n) Double
fazAlgoAi = S.cons 2

singleNeuronWeightsAndBias :: Int -> IO Double -> IO (Double, Unsized.Vector Double)
singleNeuronWeightsAndBias prevSize generator = 
    (,) <$> generator <*> (Unsized.fromList <$> replicateM prevSize generator)

layerWeightsAndBias :: IO Double -> Int -> Int -> IO (Unsized.Vector (Double, Unsized.Vector Double))
layerWeightsAndBias generator prevSize curSize  =
    Unsized.fromList <$> replicateM curSize (singleNeuronWeightsAndBias prevSize generator)

getLayerSizes :: IO Int
getLayerSizes = read <$> getLine

someIOAction :: IO ()
someIOAction = do
    putStrLn "Input brain structure"
    S.SomeSized singleLayer <- getLayerSizes >>= layerWeightsAndBias randomIO 3
    print $ nuTap singleLayer