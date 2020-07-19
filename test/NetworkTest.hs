{-# LANGUAGE FlexibleInstances #-}

module NetworkTest where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Types.Network
import Logic.Network as L.Network
import IO.Network
import Control.Monad
import qualified Data.Vector as V

ourBrain :: [[Neuron]]
ourBrain = [[Neuron 1.0 [0.2,0.3,0.4],
             Neuron 1.0 [0.5,0.6,0.7],
             Neuron 1.0 [0.8,0.9,1.0],
             Neuron 1.0 [1.1,1.2,1.3]],
            
            [Neuron 1.0 [1.4,1.5,1.6,1.7],
             Neuron 1.0 [1.8,1.9,2.0,2.1]]]

ourBrainV :: V.Vector (V.Vector UV_Neuron)
ourBrainV = V.fromList [V.fromList [UV_Neuron 1.0 (V.fromList [0.2,0.3,0.4]),
                                    UV_Neuron 1.0 (V.fromList [0.5,0.6,0.7]),
                                    UV_Neuron 1.0 (V.fromList [0.8,0.9,1.0]),
                                    UV_Neuron 1.0 (V.fromList [1.1,1.2,1.3])],
             
                        V.fromList [UV_Neuron 1.0 (V.fromList [1.4,1.5,1.6,1.7]),
                                    UV_Neuron 1.0 (V.fromList [1.8,1.9,2.0,2.1])]]


tests :: TestTree
tests = testGroup "Network Tests" [smallLogicTests, bigLogicTests]

properties :: TestTree
properties = testGroup "Property tests" [{- propertyTests -}]

smallLogicTests :: TestTree
smallLogicTests = testGroup "Small logic"
  [ testCase "Weight input calculation is correct" $
      L.Network.weightInputs [Activation 1.0 undefined, Activation 2.0 0] Neuron { bias = 0.0, inWeights = [2.0, 3.0] } @?= 8.0

  , testCase "feeding a single neuron works" $
      L.Network.feedNeuron [Activation 1.0 0, Activation 2.0 0] Neuron { bias = 1.0, inWeights = [0.001, 0.001] } @?= Activation 0.7316480054113164 1.003

  , testCase "feeding a single neuronV works" $
    L.Network.feedNeuronV (V.fromList [UV_Activation 1.0 0, UV_Activation 2.0 0]) UV_Neuron { uv_bias = 1.0, uv_inWeights = V.fromList [0.001, 0.001] } @?= UV_Activation 0.7316480054113164 1.003
  ]

bigLogicTests :: TestTree
bigLogicTests = testGroup "Big, \"controller-like\" logic"
  [ testCase "feedBrain returns correct result in last layer" $
    (last . L.Network.feedBrain [1,1,1]) ourBrain @?= [Activation {activation = 0.998969150393322, weightedInput = 6.8763405746014925}, Activation {activation = 0.9997724097287007, weightedInput = 8.387735985182916}],

    testCase "feedBrain returns correct result in last layer" $
    (V.last . L.Network.feedBrainV (V.fromList [1,1,1])) ourBrainV @?= V.fromList [UV_Activation {uv_activation = 0.998969150393322, uv_weightedInput = 6.8763405746014925}, UV_Activation {uv_activation = 0.9997724097287007, uv_weightedInput = 8.387735985182916}]
  ]

{- Property based tests reveal some improvements that could be made:
   1. Make sigmoid learning more reliable, possibly with batching?
   2. Should be easier to switch activation functions. Maybe passing around in parameter from beginning, or include in neuron data structure (bleh) 
   3. In fact, using relu makes the property tests *much* more reliable. The desired output can even be completely random and it guarantees a better result always. -}
-- instance {-# OVERLAPPING #-} Arbitrary [[Neuron]] where
--   arbitrary = do 
--     layerSizes <- map ((+1) . abs) <$> QC.listOf1 QC.arbitrarySizedIntegral -- to guarantee positive integers
--     IO.Network.makeBrain (abs <$> arbitrarySizedFractional) (fmap (abs <$>) arbitrarySizedFractional) layerSizes

-- propertyTests :: TestTree
-- propertyTests = testGroup "Network learns"
--   [ QC.testProperty "learned brain should classify better no matter what" $
--     \brain input desired ->
--         length brain >= 2 && 
--         length input >= length (inWeights $ head $ head brain) && 
--         length desired >= length (last brain)
--         QC.==>
--         let 
--             acts        = (last . L.Network.feedBrain input) brain
--             err         = L.Network.errorLast acts desired
--             learned     = L.Network.learn brain input desired
--             actsLearned = (last . L.Network.feedBrain input) learned
--             errLearned  = L.Network.errorLast actsLearned desired
--             in
--                 abs (foldl (+) 0.0 errLearned) < abs (foldl (+) 0.0 err)
--   ]