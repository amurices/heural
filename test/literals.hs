module Literals where

import Types

neuron0, neuron1, neuron2 :: (Bias, [Weight])
neuron0 = (0, [1,1,1])
neuron1 = (0, [2,2,2])
neuron2 = (0, [1,2,3])

layer0 :: Layer
layer0 = [neuron0, neuron1]

layer1 :: Layer
layer1 = [neuron1, neuron2, neuron0]
