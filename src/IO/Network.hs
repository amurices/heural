module IO.Network where 

import System.Random
import Control.Monad
import qualified Logic.Maths as LM
import Types.Network (Neuron(..))
import Logic.Network

gauss :: Monad m => Double -> m Double -> m Double
gauss scale gen = do
  x1 <- gen
  x2 <- gen
  return $ scale * LM.boxMuller x1 x2

-- Generates a layer of ncur neurons, with each having nprev random input Doubles
makeLayer :: IO Double -> Either Double (IO Double) -> Int -> Int -> IO [Neuron]
makeLayer gen bias nprev ncur = replicateM ncur biasedNeuron
  where biasedNeuron = case bias of Right x -> Neuron <$> x <*> replicateM nprev gen
                                    Left x  -> Neuron     x <$> replicateM nprev gen

makeBrain :: IO Double -> Either Double (IO Double) -> [Int] -> IO [[Neuron]]
makeBrain gen biasGen ints = tail <$> zipWithM (makeLayer (gauss 0.01 randomIO) (Right (gauss 0.01 randomIO))) (1:ints) ints
