module IO.Network where 

import System.Random
import Control.Monad
import qualified Logic.Maths as LM
import Types.Network
import Logic.Network

gauss :: Double -> IO Double
gauss scale = do
  x1 <- randomIO
  x2 <- randomIO
  return $ scale * LM.boxMuller x1 x2

-- Generates a layer of ncur neurons, with each having nprev random input Doubles
makeLayer :: Int -> Int -> IO [Neuron]
makeLayer nprev ncur = replicateM ncur $ Neuron 1.0 <$> replicateM nprev (gauss 0.01)

makeBrain :: [Int] -> IO [[Neuron]]
makeBrain ints = zipWithM makeLayer (1:ints) ints
