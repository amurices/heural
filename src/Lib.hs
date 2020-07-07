module Lib
    ( someFunc
    )
where

import           IO.Network
import qualified Logic.Network                 as LN
import           Types.Network
import           Control.Monad
import           System.Random
import           System.Environment
import           Codec.Compression.GZip         ( decompress )
import qualified Data.ByteString.Lazy          as B
import           Data.List
import           Util

mnistLabelsMagicNumber :: B.ByteString
mnistLabelsMagicNumber = B.pack [0, 0, 8, 1]

mnistImagesMagicNumber :: B.ByteString
mnistImagesMagicNumber = B.pack [0, 0, 8, 3]

readAndValidateMNISTLabels :: IO [Integer]
readAndValidateMNISTLabels = do
    labels <- decompress <$> B.readFile "datasets/mnist/train-labels-idx1-ubyte.gz"
    unless (mnistLabelsMagicNumber `B.isPrefixOf` labels) $
        error "wrong magic number; this isn't MNIST labels"
    pure . take 10000 . map fromIntegral . B.unpack . B.drop 8 $ labels

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

-- |Each list will have dimension0 * dimension1 elements
readAndValidateMNISTImages :: IO [[Integer]]
readAndValidateMNISTImages = do
    images <- decompress <$> B.readFile "datasets/mnist/train-images-idx3-ubyte.gz"
    unless (mnistImagesMagicNumber `B.isPrefixOf` images) $
        error "wrong magic number; this isn't MNIST images"
    let bytestringToInteger = B.foldl (\acc x -> acc * 256 + fromIntegral x) 0
        dimension0 = bytestringToInteger $ B.take 4 $ B.drop 8 images
        dimension1 = bytestringToInteger $ B.take 4 $ B.drop 12 images
    pure . take 20000 . splitEvery (dimension0 * dimension1) . map fromIntegral . B.unpack . B.drop 8 $ images

iteration :: [[Neuron]] -> [Integer] -> Integer -> [[Neuron]]
iteration brain image label = LN.learn brain image' label'
  where label' = replicate (fromInteger label) 0.0 ++ 1.0 : replicate (9 - fromInteger label) 0.0
        image' = map ((/ 256.0) . fromIntegral) image

manyIterations :: [[Neuron]] -> [[Integer]] -> [Integer] -> [[Neuron]]
manyIterations brain images labels = let
    twoTogether = zip images labels
    in foldl' (\evolvingBrain (img, lab) -> iteration evolvingBrain img lab) brain twoTogether
 
someFunc :: IO ()
someFunc = do
    labels <- readAndValidateMNISTLabels
    images <- readAndValidateMNISTImages
    putStrLn "Input the size of each layer of the brain"
    sizes <- map read . words <$> getLine
    brain <- makeBrain randomIO (Right randomIO) sizes
    let evolvedBrain = manyIterations brain images labels
        image'       = map ((/ 256.0) . fromIntegral) (head images)
    nuTap (last $ LN.feedBrain image' evolvedBrain) `seq` putStrLn "o" 