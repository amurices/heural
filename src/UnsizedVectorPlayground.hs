{-# LANGUAGE DataKinds, TypeOperators #-}

module UnsizedVectorPlayground where

import qualified Data.Vector.Sized as S
import qualified Data.Vector as Unsized
import GHC.TypeLits
import Control.Monad
import Util
import System.Random

data UVNeuronPlay = Neuron {bias :: Double, inWeights :: Unsized.Vector Double}
  deriving (Eq, Show)

data UVActivationPlay n = Activation {activation :: Double, weightedInput ::  Unsized.Vector Double}
  deriving (Show, Eq)

singleNeuronWeightsAndBias :: Int -> IO Double -> IO UVNeuronPlay
singleNeuronWeightsAndBias prevSize generator = 
    Neuron <$> generator <*> (Unsized.fromList <$> replicateM prevSize generator)

layerWeightsAndBias :: IO Double -> Int -> Int -> IO (Unsized.Vector UVNeuronPlay)
layerWeightsAndBias generator prevSize curSize  =
    Unsized.fromList <$> replicateM curSize (singleNeuronWeightsAndBias prevSize generator)

makeLayers :: IO Double -> [Int] -> IO (Unsized.Vector (Unsized.Vector UVNeuronPlay))
makeLayers generator sizes = Unsized.fromList . tail <$> zipWithM (layerWeightsAndBias generator) (1:sizes) sizes 

getLayerSizes :: IO [Int]
getLayerSizes = map read . words <$> getLine

someIOAction :: IO ()
someIOAction = do
    putStrLn "Input brain structure"
    brain <- getLayerSizes >>= makeLayers randomIO
    print $ nuTap brain