module Lib
    ( someFunc
    )
where

import           IO.Network
import qualified Logic.Network                 as LN
import           Types.Network(activation)
import           Control.Monad
import           System.Random
import           System.Environment
import           Codec.Compression.GZip         ( decompress )
import qualified Data.ByteString.Lazy          as B
import           Data.List
import           Util
import           Text.Printf

mnistLabelsMagicNumber :: B.ByteString
mnistLabelsMagicNumber = B.pack [0, 0, 8, 1]

mnistImagesMagicNumber :: B.ByteString
mnistImagesMagicNumber = B.pack [0, 0, 8, 3]

readAndValidateMNISTLabels :: Int -> IO [Integer]
readAndValidateMNISTLabels subset = do
    labels <- decompress
        <$> B.readFile "datasets/mnist/train-labels-idx1-ubyte.gz"
    unless (mnistLabelsMagicNumber `B.isPrefixOf` labels)
        $ error "wrong magic number; this isn't MNIST labels"
    pure . take subset . map fromIntegral . B.unpack . B.drop 8 $ labels

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as, bs) = splitAt n xs

-- |Each list will have dimension0 * dimension1 elements
readAndValidateMNISTImages :: Int -> IO [[Integer]]
readAndValidateMNISTImages subset = do
    images <- decompress <$> B.readFile "datasets/mnist/train-images-idx3-ubyte.gz"
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
    putStrLn "How many cases to test on?"
    numCases <- read <$> getLine
    labels <- readAndValidateMNISTLabels numCases
    images <- readAndValidateMNISTImages numCases
    let inputLayerSize  = length (head images) 
        outputLayerSize = 10
    putStrLn $ printf "Network'll look like this: %d,_,_,...,_,%d\nInput the size of layers" inputLayerSize outputLayerSize
    sizes <- map read . words <$> getLine
    brain <- makeLayers randomIO (length (head images) : sizes ++ [outputLayerSize])
    let evolvedBrain = LN.learnMany brain (map imageToInput images) (map labelToDesired labels)
    forever $ do
        putStrLn $ "Mention a training case from 0 to " ++ show (numCases-1)
        ind <- read <$> getLine
        let image' = imageToInput (images !! ind)
        putStrLn $ printf "%dth case is a %d:\n" ind (labels !! ind) ++ asciiImage 28 image'
        nuTap (sortBy (\x y -> compare (snd y) (snd x))  $ zip [0..] $ map activation $ last $ LN.feedBrain image' evolvedBrain) `seq` putStrLn "o"
