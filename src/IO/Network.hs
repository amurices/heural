module IO.Network where 

import System.Random
import Control.Monad
import qualified Logic.Maths as LM
import Types.Network (Neuron(..))
import Logic.Network

gauss :: Double -> IO Double
gauss scale = do
  x1 <- randomIO
  x2 <- randomIO
  return $ scale * LM.boxMuller x1 x2

-- Generates a layer of ncur neurons, with each having nprev random input Doubles
makeLayer :: (Monad m) => m Double -> Either Double (m Double) -> Int -> Int -> m [Neuron]
makeLayer gen bias nprev ncur = replicateM ncur biasedNeuron
  where biasedNeuron = case bias of Right x -> Neuron <$> x <*> replicateM nprev gen
                                    Left x  -> Neuron     x <$> replicateM nprev gen

makeBrain :: (Monad m) => m Double -> Either Double (m Double) -> [Int] -> m [[Neuron]]
makeBrain gen biasGen ints = tail <$> zipWithM (makeLayer gen biasGen) (1:ints) ints
