{-
-- RatCalc - An infinite precision numeric computation framework
-- Copyright (C) 2011 Jim Farrand
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
 - Implementation of real numbers, as either exact rationals, signed
 - binary digit stream representation.
 -}

module RatCalc.Number.ExactReal
    ( ExactReal
    , fromSBDSR
    ) where

import RatCalc.Number.SignedBinaryDigitStreamRepresentation
import RatCalc.Estimator
import RatCalc.Arithmetic
import Data.Ratio

data ExactReal = RRational Rational | RSBDSR SBDSR
    deriving (Show)

fromSBDSR = RSBDSR

instance Eq ExactReal where
    RRational a == RRational b = a == b
    a == b = toSBDSR a == toSBDSR b

instance Ord ExactReal where
    compare (RRational a) (RRational b) = compare a b
    compare a b = compare (toSBDSR a) (toSBDSR b)

instance Num ExactReal where
    fromInteger = RRational . fromInteger

    RRational a + RRational b = RRational (a+b)
    RSBDSR a + RRational b = RSBDSR (a + fromRational b)
    RRational a + RSBDSR b = RSBDSR (fromRational a + b)
    RSBDSR a + RSBDSR b = RSBDSR (a+b)

    RRational a - RRational b = RRational (a-b)
    RSBDSR a - RRational b = RSBDSR (a - fromRational b)
    RRational a - RSBDSR b = RSBDSR (fromRational a - b)
    RSBDSR a - RSBDSR b = RSBDSR (a-b)

    RRational a * (RRational b) = RRational (a*b)
    RSBDSR a * (RRational b) = RSBDSR (a * fromRational b)
    RRational a * (RSBDSR b) = RSBDSR (fromRational a * b)
    RSBDSR a * (RSBDSR b) = RSBDSR (a*b)

instance IntegerDivision ExactReal where
    (RRational a) /# b = RRational (a * (1%b))
    (RSBDSR a) /# b = RSBDSR (a /# b)

instance Fractional ExactReal where
    fromRational = RRational

instance ToSBDSR ExactReal where
    toSBDSR (RSBDSR n) = n
    toSBDSR (RRational n) = fromRational n

instance Estimator ExactReal where
    toNestedIntervals (RRational a) = [(a,a)]
    toNestedIntervals (RSBDSR a) = toNestedIntervals a
