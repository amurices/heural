module Util where

import Debug.Pretty.Simple
import Text.Pretty.Simple.Internal.OutputPrinter
import Text.Pretty.Simple.Internal.Color
import qualified Logic.Maths as LM

gauss :: Monad m => Double -> m Double -> m Double
gauss scale gen = do
  x1 <- gen
  x2 <- gen
  return $ scale * LM.boxMuller x1 x2

nuColors :: ColorOptions
nuColors = 
  ColorOptions {colorQuote = colorDullMagenta,
                colorString = colorDullMagenta,
                colorError = colorVividMagentaBold,
                colorNum = colorDullCyan,
                colorRainbowParens = defaultColorRainbowParensDarkBg}

nuTap :: (Show x) => x -> x
nuTap x = pTraceShowOpt CheckColorTty
                        OutputOptions 
                        {outputOptionsIndentAmount = 2,
                         outputOptionsColorOptions = Just nuColors,
                         outputOptionsEscapeNonPrintable = False} x x `seq` x

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as, bs) = splitAt n xs