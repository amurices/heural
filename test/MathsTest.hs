module MathsTest where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Types.Network (Activation(..))
import Logic.Maths as L.Maths

tests :: TestTree 
tests = testGroup "Maths test" [arbitraryMathTestsSeriouslyTheseAreJustTotallyRandomlyChosenFns]


arbitraryMathTestsSeriouslyTheseAreJustTotallyRandomlyChosenFns :: TestTree
arbitraryMathTestsSeriouslyTheseAreJustTotallyRandomlyChosenFns = testGroup "Some random fns from Logic.Maths"
  [ testCase "Vector length calculation is correct" $
      L.Maths.vectorLength [-4, -12, 2] @?= 12.806248474865697 -- this would be better as a "roughly" matcher

  , testCase "quadratic cost function calculation is correct" $
      L.Maths.quadratic [2, -5, 4] [6, 7, 2] @?= 164
  ]
