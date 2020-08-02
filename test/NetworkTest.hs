module NetworkTest where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Types.Network
import Logic.Network as L.Network
import Logic.Maths
import IO.Network
import Control.Monad

import Arbitraries

ourNetwork :: Double -> (Double -> Double) -> (Double -> Double) -> Network
ourNetwork eta actFn actFn' = 
  Network {eta = eta,
           activationFunction = actFn,
           activationFunction' = actFn',
           net = [[Neuron 1.0 [0.2,0.3,0.4],
                   Neuron 1.0 [0.5,0.6,0.7],
                   Neuron 1.0 [0.8,0.9,1.0],
                   Neuron 1.0 [1.1,1.2,1.3]],
                  
                  [Neuron 1.0 [1.4,1.5,1.6,1.7],
                   Neuron 1.0 [1.8,1.9,2.0,2.1]]]}

defaultEta :: Double
defaultEta = 0.002

tests :: TestTree
tests = testGroup "Network Tests" [smallLogicTests, bigLogicTests]

properties :: TestTree
properties = testGroup "Property tests" [propertyTests]

smallLogicTests :: TestTree
smallLogicTests = testGroup "Small logic"
  [ testCase "Weight input calculation is correct" $
      L.Network.weightInputs [Activation 1.0 undefined, Activation 2.0 0] Neuron { bias = 0.0, inWeights = [2.0, 3.0] } @?= 8.0

  , testCase "feeding a single neuron works" $
      L.Network.feedNeuron sigmoid [Activation 1.0 0, Activation 2.0 0] Neuron { bias = 1.0, inWeights = [0.001, 0.001] } @?= Activation 0.7316480054113164 1.003
  ]

bigLogicTests :: TestTree
bigLogicTests = testGroup "Big, \"controller-like\" logic"
  [ testCase "feedNetwork returns correct result in last layer" $
    (last . L.Network.feedNetwork [1,1,1]) (ourNetwork defaultEta sigmoid sigmoid') @?= [Activation {activation = 0.998969150393322, weightedInput = 6.8763405746014925}, Activation {activation = 0.9997724097287007, weightedInput = 8.387735985182916}]
  ]

-- Both ReLU and Leaky ReLU fail this property sometimes.
propertyTests :: TestTree
propertyTests = testGroup "Network learns"
  [ QC.testProperty "learned network should classify better no matter what" $
    \network input desired ->
      let 
        acts        = (last . L.Network.feedNetwork input) network
        err         = L.Network.errorLast (activationFunction' network) acts desired
        learned     = L.Network.learn network input desired
        actsLearned = (last . L.Network.feedNetwork input) learned
        errLearned  = L.Network.errorLast (activationFunction' network) actsLearned desired
        in
          abs (sum errLearned) < abs (sum err)
  ]