
module RatCalc.Arithmetic where

import Data.Ratio

infixl 7 /#

class IntegerDivision a where
    (/#) :: a -> Integer -> a

instance Integral a => IntegerDivision (Ratio a) where
    a /# b = a * (1%fromInteger b)
