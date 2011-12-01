
module RatCalc.ConversionUtils where

integerToBits n
    | n >= 0 = reverse (integerToBits' n)
    where
        integerToBits' n
            | n == 0 = []
            | odd n = True : integerToBits' (n `div` 2)
            | otherwise = False : integerToBits' (n `div` 2)
