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
    makeLayers (abs . noNaN <$>  QC.arbitrary) layerSizes
    where noNaN x = if isNaN x then 1.0 else x

instance {-# OVERLAPPING #-} Arbitrary [Double] where
  arbitrary = do
    arbitraryInteger <- arbitrary
    replicateM arbitraryInteger (fmap ((+ 1) . abs . noNaN) arbitrary)
    where noNaN x = if isNaN x then 1.0 else x
