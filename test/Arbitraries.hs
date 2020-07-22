{-# LANGUAGE FlexibleInstances #-}

module Arbitraries where

import Types.Network
import IO.Network
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Control.Monad

instance {-# OVERLAPPING #-} Arbitrary [[Neuron]] where
  arbitrary = do
    layerSizes <- do
      firstElem <- QC.arbitrary
      secondPart <- QC.listOf1 QC.arbitrary
      pure $ map (succ . abs) (firstElem : secondPart)
    makeLayers (abs <$> QC.arbitrary) layerSizes

instance {-# OVERLAPPING #-} Arbitrary [Double] where
  arbitrary = do
    arbitraryInteger <- arbitrary
    replicateM arbitraryInteger (fmap ((+ 1) . abs) arbitrary)
