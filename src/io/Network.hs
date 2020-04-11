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

-- Generates a layer of ncur neurons, with each having nprev random input weights
makeLayer :: Int -> Int -> IO Layer
makeLayer nprev ncur = fmap Layer $ replicateM ncur $ Neuron 1.0 <$> replicateM nprev (gauss 0.01)

makeBrain :: [Int] -> IO Brain
makeBrain ints = Brain <$> zipWithM makeLayer (1:ints) ints
