module MathsTest where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Logic.Maths

tests :: TestTree 
tests = testGroup "Maths test" []