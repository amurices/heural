module Util 
  (nuTap) where

import Debug.Pretty.Simple
import Text.Pretty.Simple.Internal.OutputPrinter
import Text.Pretty.Simple.Internal.Color


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
                         outputOptionsEscapeNonPrintable = False} x x
