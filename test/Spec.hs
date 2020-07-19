module Main where

import qualified NetworkTest as NT
import qualified MathsTest as MT

import Test.Tasty

tests :: TestTree
tests = testGroup "Tests" [NT.tests, {- NT.properties, -} MT.tests]

main :: IO ()
main = defaultMain tests
