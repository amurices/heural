{-# LANGUAGE FlexibleInstances #-}

module Arbitraries where

import Types.Network
import IO.Network
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Control.Monad
import Util

instance {-# OVERLAPPING #-} Arbitrary [[Neuron]] where
  arbitrary = do
    layerSizes <- do
      firstElem <- QC.arbitrary
      secondPart <- QC.listOf1 QC.arbitrary
      pure $ map (succ . abs) (firstElem : secondPart) -- networks should be at least 2 long
    makeNetwork (abs <$>  QC.arbitrary) layerSizes

instance {-# OVERLAPPING #-} Arbitrary [Double] where
  arbitrary = do
    positiveInteger <- ((+ 1) . abs) <$> arbitrary 
    replicateM positiveInteger (fmap ((+ 1) . abs) arbitrary)
