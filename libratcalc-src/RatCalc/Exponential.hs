
module RatCalc.Exponential where

import RatCalc.Number.SignedBinaryDigitStreamRepresentation
import RatCalc.Number.RealNumber
import RatCalc.Cauchy as Cauchy
import RatCalc.Limits
import Data.Ratio
import RatCalc.Estimator
import RatCalc.Interval
import RatCalc.Arithmetic

e = exponential (1 :: Rational)

exponential x = limit (exponentialIntervals x)

exponentialIntervals :: (Estimator a, IntegerDivision a, Ord a, Num a) => a -> [Interval a] 
exponentialIntervals x = map (\(a,b) -> makeInterval a (a+b)) (exponentialApproximations x)

exponentialApproximations :: (Ord a, Estimator a, Num a, IntegerDivision a) => a -> [(a,a)]
exponentialApproximations x = exponentialApproximations' 0 1 1 0 False
    where
      exponentialApproximations' a n d i o
        | output = (a', v) : exponentialApproximations' a' n' d' i' True
        | otherwise  = exponentialApproximations' a' n' d' i' False
        where
          output = o || isLargeEnough n d
          v = n /# d
          a' = a + v
          n' = n * x
          i' = i+1
          d' = d * i'

          isLargeEnough :: (Ord a, Estimator a, Num a) => a -> Integer -> Bool
          isLargeEnough n d = not (approxEqual (1%1000) n (fromInteger d)) && n < fromInteger d
