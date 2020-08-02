-- TODO: Move this to an example executable

module Lib
    ( someFunc
    )
where

import           IO.Network
import qualified Logic.Network                 as LN
import           Types.Network(activation)
import           Control.Monad
import           System.Random
import           Codec.Compression.GZip         ( decompress )
import qualified Data.ByteString.Lazy          as B
import           Data.List
import           Util
import           Text.Printf
import           Logic.Maths

mnistLabelsMagicNumber :: B.ByteString
mnistLabelsMagicNumber = B.pack [0, 0, 8, 1]

mnistImagesMagicNumber :: B.ByteString
mnistImagesMagicNumber = B.pack [0, 0, 8, 3]

readMNISTLabels :: FilePath -> Int -> IO [Integer]
readMNISTLabels filepath subset = do
    labels <- decompress
        <$> B.readFile filepath
    unless (mnistLabelsMagicNumber `B.isPrefixOf` labels)
        $ error "wrong magic number; this isn't MNIST labels"
    pure . take subset . map fromIntegral . B.unpack . B.drop 8 $ labels

-- Each list will have dimension0 * dimension1 elements
readMNISTImages :: FilePath -> Int -> IO [[Integer]]
readMNISTImages filepath subset = do
    images <- decompress <$> B.readFile filepath
    unless (mnistImagesMagicNumber `B.isPrefixOf` images)
        $ error "wrong magic number; this isn't MNIST images"
    let bytestringToInteger = B.foldl (\acc x -> acc * 256 + fromIntegral x) 0
        dimension0          = bytestringToInteger $ B.take 4 $ B.drop 8 images
        dimension1          = bytestringToInteger $ B.take 4 $ B.drop 12 images
    pure . take subset
         . splitEvery (dimension0 * dimension1)
         . map fromIntegral
         . B.unpack
         . B.drop 16
         $ images

asciiImage :: Int -> [Double] -> String
asciiImage rowsize image =
  let lums = ".,-~:;=!*#$@"
      doubleToCharLum x = lums !! truncate ratio
              where ratio = x * (fromIntegral $ length lums :: Double)
      luminanceList = map doubleToCharLum image
  in unlines $ splitEvery rowsize luminanceList

labelToDesired :: Integer -> [Double]
labelToDesired label = replicate (fromInteger label) 0.0 ++ 1.0 : replicate (9 - fromInteger label) 0.0

imageToInput :: [Integer] -> [Double]
imageToInput = map ((/ 256.0) . fromIntegral)

someFunc :: IO ()
someFunc = do
    putStrLn "How many cases to train on?"
    numCases <- read <$> getLine
    trainingLabels <- readMNISTLabels "datasets/mnist/train-labels-idx1-ubyte.gz" numCases
    trainingImages <- readMNISTImages "datasets/mnist/train-images-idx3-ubyte.gz" numCases
    testLabels <- readMNISTLabels "datasets/mnist/t10k-labels-idx1-ubyte.gz" 10000
    testImages <- readMNISTImages "datasets/mnist/t10k-images-idx3-ubyte.gz" 10000
    let inLayerSize  = length (head trainingImages) 
        outLayerSize = 10
    putStrLn $ printf "Network'll look like this: %d,_,_,...,_,%d\nWhat should be the sizes of middle layers?" inLayerSize outLayerSize
    layerSizes <- (++ [outLayerSize]) . (inLayerSize : ) . map read . words <$> getLine
    let eta = 0.002
    network <- makeNetwork eta relu relu' randomIO layerSizes
    let evolvedNetwork = LN.learnMany network (map imageToInput trainingImages) (map labelToDesired trainingLabels)

    let accuracy = undefined -- TODO: Final touch on this example program for mnist is to show accuracy for given params

    forever $ do
        putStrLn $ "Mention a test case from 0 to " ++ show 9999
        ind <- read <$> getLine
        let image' = imageToInput (testImages !! ind)
        putStrLn $ printf "%dth case is a %d:\n" ind (testLabels !! ind) ++ asciiImage 28 image'
        print $ sortBy (\x y -> compare (snd y) (snd x))  $ zip [0..] $ map activation $ last $ LN.feedNetwork image' evolvedNetwork
