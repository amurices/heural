module NetworkTest where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Types.Network
import qualified Logic.Network as L.Network

ourBrain :: [[Neuron]]
ourBrain = [[Neuron 1.0 [0.2,0.3,0.4],
             Neuron 1.0 [0.5,0.6,0.7],
             Neuron 1.0 [0.8,0.9,1.0],
             Neuron 1.0 [1.1,1.2,1.3]],
            
            [Neuron 1.0 [1.4,1.5,1.6,1.7],
             Neuron 1.0 [1.8,1.9,2.0,2.1]]]

tests :: TestTree
tests = testGroup "Network Tests" [smallLogicTests, bigLogicTests]

properties :: TestTree
properties = testGroup "Property tests" [propertyTests]

smallLogicTests :: TestTree
smallLogicTests = testGroup "Small logic"
  [ testCase "Weight input calculation is correct" $
      L.Network.weightInputs [Activation 1.0 undefined, Activation 2.0 0] Neuron { bias = 0.0, inWeights = [2.0, 3.0] } @?= 8.0

  , testCase "feeding a single neuron works" $
      L.Network.feedNeuron [Activation 1.0 0, Activation 2.0 0] Neuron { bias = 1.0, inWeights = [0.001, 0.001] } @?= Activation 0.7316480054113164 1.003
  ]

bigLogicTests :: TestTree
bigLogicTests = testGroup "Big, \"controller-like\" logic"
  [ testCase "feedBrain returns correct result in last layer" $
    (last . L.Network.feedBrain [1,1,1]) ourBrain @?= [Activation {activation = 0.998969150393322, weightedInput = 6.8763405746014925}, Activation {activation = 0.9997724097287007, weightedInput = 8.387735985182916}]
  ]

instance Arbitrary Neuron where
  arbitrary = QC.applyArbitrary2 Neuron

propertyTests :: TestTree
propertyTests = testGroup "Network learns"
  [ QC.testProperty "learned brain should classify better no matter what" $
    \brain input desired ->
        not (null brain) && -- TODO: How to do this in the type level?
        all (not . null) brain && 
        all (\layer -> all (\neuron -> not . null . inWeights $ neuron) layer) brain &&
        not (null input) &&
        not (null desired) && 
        length desired == length (last brain)
        QC.==>
        let acts        = (last . L.Network.feedBrain input) brain
            err         = L.Network.errorLast acts desired
            learned     = L.Network.learn brain input desired
            actsLearned = (last . L.Network.feedBrain input) learned
            errLearned  = L.Network.errorLast actsLearned desired
            in
                abs (foldl (+) 0.0 errLearned) < abs (foldl (+) 0.0 err)

  ]
-- TODO: What's best way to implement passing around different activation functions? For example with relu:
-- (last . L.Network.feedBrain relu [1,1,1]) ourBrain @?= [Activation {activation = 21.6, weightedInput = 21.6}, Activation {activation = 26.8, weightedInput = 26.8}]
