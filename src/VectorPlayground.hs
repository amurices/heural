{-# LANGUAGE DataKinds, TypeOperators #-}

module VectorPlayground where

import qualified Data.Vector.Sized as S
import qualified Data.Vector as Unsized
import qualified Data.Vector.Fixed.Boxed as Boxed
import qualified Data.Vector.Fixed as Fixed
import GHC.TypeLits

data VNeuron n = VNeuron {bias :: Double, inWeights :: S.Vector n Double}
  deriving (Eq, Show)

data Activation2 = Activation2 {activation :: Double,
                                weightedInput :: Double}
  deriving (Show, Eq)

getAMatrixIO :: IO (Unsized.Vector (Unsized.Vector Double))
getAMatrixIO = do
    ints <- map read . words <$> getLine
    pure $ Unsized.fromList ints

fazAlgoAi :: S.Vector n Double -> S.Vector (1 + n) Double
fazAlgoAi = S.cons 2

layerFromSize :: S.Vector n Double -> S.Vector n (S.Vector m Double) -> S.Vector n (VNeuron m)
layerFromSize = S.zipWith VNeuron

getLayerSizes :: IO (Unsized.Vector Int)
getLayerSizes = do
    ints <- map read . words <$> getLine
    pure $ Unsized.fromList ints

genWeightsFromSizes :: Unsized.Vector Int -> IO (Unsized.Vector (Unsized.Vector Double))
genWeightsFromSizes sizes = undefined

someIOAction :: IO ()
someIOAction = do
    putStrLn "Input brain structure"
    S.SomeSized brain <- getLayerSizes
    S.SomeSized v <- getAMatrixIO -- v is `Sized.Vector n Int`
    print "sei la"