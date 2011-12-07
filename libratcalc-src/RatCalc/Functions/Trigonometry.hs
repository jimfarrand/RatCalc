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

{-
 - Trigonemtric functions
 -}

module RatCalc.Functions.Trigonometry where

import RatCalc.Number.SignedBinaryDigitStreamRepresentation
import RatCalc.Number.ExactReal
import RatCalc.Cauchy as Cauchy
import RatCalc.Limits
import RatCalc.Arithmetic
import Data.Ratio

arctan :: (Ord a, Num a, IntegerDivision a, ToSBDSR a) => a -> ExactReal
arctan x = limitsToExactReal (Cauchy.ranges (converging (arctanIntervals x)))
    where
        arctanIntervals x = arctan' True x 1 0
          where
            arctan' s p d a =
              a' : arctan' (not s) ((x * x) * p) (d+2) a'
              where
                t = p /# d
                v = if s
                      then t
                      else (negate t)
                a' = a + v

pi = 24*arctan (r (1%8)) + 8*arctan (r (1%57)) + 4*arctan (r (1%239))
    where
        r :: Rational -> Rational
        r = id
