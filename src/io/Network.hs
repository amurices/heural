module IO.Network where 

import System.Random
import Control.Monad
import qualified Logic.Maths as LM
import Types.Network
import Logic.Network

gauss :: Float -> IO Float
gauss scale = do
    x1 <- randomIO
    x2 <- randomIO
    return $ scale * LM.boxMuller x1 x2

makeLayer :: Int -> IO Layer
makeLayer x = fmap Layer $ replicateM x $ fmap (Neuron 1.0) $ replicateM x (gauss 0.01)

makeBrain :: [Int] -> IO Brain
makeBrain ints = fmap Brain $ fmap tail $ mapM makeLayer (1:ints)
