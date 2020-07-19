{-# LANGUAGE BangPatterns #-}

module Lib
    ( someFunc
    , someFuncV
    , someFuncSingleInput
    , asciiImage
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
import qualified Data.Vector as V
import           Util

mnistLabelsMagicNumber :: B.ByteString
mnistLabelsMagicNumber = B.pack [0, 0, 8, 1]

mnistImagesMagicNumber :: B.ByteString
mnistImagesMagicNumber = B.pack [0, 0, 8, 3]

readAndValidateMNISTLabels :: Maybe Int -> IO [Integer]
readAndValidateMNISTLabels subset = do
    labels <- decompress
        <$> B.readFile "datasets/mnist/train-labels-idx1-ubyte.gz"
    unless (mnistLabelsMagicNumber `B.isPrefixOf` labels)
        $ error "wrong magic number; this isn't MNIST labels"
    pure .
         (case subset of
                 Just n -> take n
                 Nothing -> id)
         . map fromIntegral . B.unpack . B.drop 8 $ labels

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as, bs) = splitAt n xs

-- |Each list will have dimension0 * dimension1 elements
readAndValidateMNISTImages :: Maybe Int -> IO [[Integer]]
readAndValidateMNISTImages subset = do
    images <- decompress <$> B.readFile "datasets/mnist/train-images-idx3-ubyte.gz"
    unless (mnistImagesMagicNumber `B.isPrefixOf` images)
        $ error "wrong magic number; this isn't MNIST images"
    let bytestringToInteger = B.foldl (\acc x -> acc * 256 + fromIntegral x) 0
        dimension0          = bytestringToInteger $ B.take 4 $ B.drop 8 images
        dimension1          = bytestringToInteger $ B.take 4 $ B.drop 12 images
    pure .
        (case subset of
            Just n -> take n
            Nothing -> id)
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

-- iteration2 :: [[Neuron]] -> [Integer] -> Integer -> IO [[Neuron]]
-- iteration2 brain image label =
--     let label' = replicate (fromInteger label) 0.0 ++ 1.0 : replicate (9 - fromInteger label) 0.0
--         image' = map ((/ 256.0) . fromIntegral) image
--         learned = LN.learn brain image' label'
--     in 
--         putStrLn ("label: " ++ show label') >>
--         putStrLn ("image:\n" ++ asciiImage image') >>
--         pure learned

labelToDesired :: Integer -> [Double]
labelToDesired label = replicate (fromInteger label) 0.0 ++ 1.0 : replicate (9 - fromInteger label) 0.0

imageToInput :: [Integer] -> [Double]
imageToInput = map ((/ 256.0) . fromIntegral)

manyLearns :: [[Neuron]] -> [[Double]] -> [[Double]] -> [[Neuron]]
manyLearns brain images labels =
  let twoTogether = zip images labels in
    foldl'(\evolvingBrain (img, lab) -> LN.learn evolvingBrain img lab) brain twoTogether

someFunc :: IO ()
someFunc = do
    putStrLn "iterations?"
    iterationNumber <- read <$> getLine
    labels <- take iterationNumber <$> readAndValidateMNISTLabels Nothing
    images <- take iterationNumber <$> readAndValidateMNISTImages Nothing
    putStrLn "Input the size of the next layers"
    sizes <- map read . words <$> getLine
    brain <- makeBrain randomIO randomIO (length (head images) : sizes)
    let !evolvedBrain = manyLearns brain (map imageToInput images) (map labelToDesired labels)
    forever $ do
        putStrLn "Mention a training case from 0 to 59999"
        ind <- read <$> getLine
        let image' = imageToInput (images !! ind)
        putStrLn $ show ind ++ "th case is a " ++ show (labels !! ind) ++ ":\n" ++ asciiImage 28 image'
        nuTap (map activation $ last $ LN.feedBrain image' evolvedBrain) `seq` putStrLn "o"

someFuncSingleInput :: IO ()
someFuncSingleInput = do
    labels <- readAndValidateMNISTLabels Nothing
    images <- readAndValidateMNISTImages Nothing
    brain <- makeBrain randomIO randomIO [784, 30, 10]
    putStrLn "Which case to train on?"
    caseChosen <- read <$> getLine
    putStrLn "iterations?"
    iterationNumber <- read <$> getLine
    let evolvedBrain = 
          manyLearns brain 
                     (replicate iterationNumber (imageToInput (images !! caseChosen)))
                     (replicate iterationNumber (labelToDesired (labels !! caseChosen)))
    forever $ do
        putStrLn "Mention a training case from 0 to 59999"
        ind <- read <$> getLine
        let image' = imageToInput (images !! ind)
        putStrLn $ show ind ++ "th case is a " ++ show (labels !! ind) ++ ":\n" ++ asciiImage 28 image'
        nuTap (map activation $ last $ LN.feedBrain image' evolvedBrain) `seq` putStrLn "o"

singleNeuronWeightsAndBias :: Int -> IO Double -> IO UV_Neuron
singleNeuronWeightsAndBias prevSize generator = 
    UV_Neuron <$> generator <*> (V.fromList <$> replicateM prevSize generator)

layerWeightsAndBias :: IO Double -> Int -> Int -> IO (V.Vector UV_Neuron)
layerWeightsAndBias generator prevSize curSize  =
    V.fromList <$> replicateM curSize (singleNeuronWeightsAndBias prevSize generator)

makeLayers :: IO Double -> [Int] -> IO (V.Vector (V.Vector UV_Neuron))
makeLayers generator sizes = V.fromList . tail <$> zipWithM (layerWeightsAndBias generator) (1:sizes) sizes 

getLayerSizes :: IO [Int]
getLayerSizes = map read . words <$> getLine

someIOAction :: IO ()
someIOAction = do
    putStrLn "Input brain structure"
    brain <- getLayerSizes >>= makeLayers randomIO
    print $ nuTap brain

labelToDesiredV :: Integer -> V.Vector Double
labelToDesiredV label = V.replicate (fromInteger label) 0.0 V.++ 1.0 `V.cons` V.replicate (9 - fromInteger label) 0.0

imageToInputV :: [Integer] -> V.Vector Double
imageToInputV = V.fromList . map ((/ 256.0) . fromIntegral)

manyLearnsV :: V.Vector (V.Vector UV_Neuron) -> V.Vector (V.Vector Double) -> V.Vector (V.Vector Double) -> V.Vector (V.Vector UV_Neuron)
manyLearnsV brain images labels =
  let twoTogether = V.zip images labels in
    foldl'(\evolvingBrain (img, lab) -> LN.learnV evolvingBrain img lab) brain twoTogether


someFuncV :: IO ()
someFuncV = do
    putStrLn "iterations?"
    iterationNumber <- read <$> getLine
    labels <- take iterationNumber <$> readAndValidateMNISTLabels Nothing
    images <- take iterationNumber <$> readAndValidateMNISTImages Nothing
    putStrLn "Input the size of layers"
    sizes <- map read . words <$> getLine
    brain <- makeLayers randomIO sizes
    let !evolvedBrain = manyLearnsV brain (V.fromList (map imageToInputV images)) (V.fromList (map labelToDesiredV labels))
    forever $ do
        putStrLn "Mention a training case from 0 to 59999"
        ind <- read <$> getLine
        let image' = imageToInput (images !! ind)
        putStrLn $ show ind ++ "th case is a " ++ show (labels !! ind) ++ ":\n" ++ asciiImage 28 image'
        nuTap (V.map uv_activation $ V.last $ LN.feedBrainV (V.fromList image') evolvedBrain) `seq` putStrLn "o"
