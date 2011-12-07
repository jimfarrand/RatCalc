{-
-- RatCalc - An infinite precision numeric computation framework
-- Copyright (C) 2010, 2011 Jim Farrand
--
-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
-- more details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/
-}

{- Implementation of exponetial, logarithm and power functions -}

module RatCalc.Exponential
    ( exponential
    , e
    ) where

import RatCalc.Number.SignedBinaryDigitStreamRepresentation
import RatCalc.Number.ExactReal
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
