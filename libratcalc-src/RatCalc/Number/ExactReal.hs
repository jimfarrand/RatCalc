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
    ( ExactReal(..)
    , fromSBDSR
    ) where

import RatCalc.Number.SignedBinaryDigitStreamRepresentation
import RatCalc.Estimator
import RatCalc.Arithmetic
import Data.Ratio

data ExactReal = RationalReal Rational | MaybeIrrationalReal SBDSR
    deriving (Show)

fromSBDSR = MaybeIrrationalReal

instance Eq ExactReal where
    RationalReal a == RationalReal b = a == b
    a == b = toSBDSR a == toSBDSR b

instance Ord ExactReal where
    compare (RationalReal a) (RationalReal b) = compare a b
    compare a b = compare (toSBDSR a) (toSBDSR b)

instance Num ExactReal where
    fromInteger = RationalReal . fromInteger

    RationalReal a + RationalReal b = RationalReal (a+b)
    MaybeIrrationalReal a + RationalReal b = MaybeIrrationalReal (a + fromRational b)
    RationalReal a + MaybeIrrationalReal b = MaybeIrrationalReal (fromRational a + b)
    MaybeIrrationalReal a + MaybeIrrationalReal b = MaybeIrrationalReal (a+b)

    RationalReal a - RationalReal b = RationalReal (a-b)
    MaybeIrrationalReal a - RationalReal b = MaybeIrrationalReal (a - fromRational b)
    RationalReal a - MaybeIrrationalReal b = MaybeIrrationalReal (fromRational a - b)
    MaybeIrrationalReal a - MaybeIrrationalReal b = MaybeIrrationalReal (a-b)

    RationalReal a * (RationalReal b) = RationalReal (a*b)
    MaybeIrrationalReal a * (RationalReal b) = MaybeIrrationalReal (a * fromRational b)
    RationalReal a * (MaybeIrrationalReal b) = MaybeIrrationalReal (fromRational a * b)
    MaybeIrrationalReal a * (MaybeIrrationalReal b) = MaybeIrrationalReal (a*b)

    abs _ = error "ExactReal.abs: not implemented"
    signum _ = error "ExactReal.abs: not implemented"

instance IntegerDivision ExactReal where
    (RationalReal a) /# b = RationalReal (a * (1%b))
    (MaybeIrrationalReal a) /# b = MaybeIrrationalReal (a /# b)

instance Fractional ExactReal where
    fromRational = RationalReal

instance ToSBDSR ExactReal where
    toSBDSR (MaybeIrrationalReal n) = n
    toSBDSR (RationalReal n) = fromRational n

instance Estimator ExactReal where
    toNestedIntervals (RationalReal a) = [(a,a)]
    toNestedIntervals (MaybeIrrationalReal a) = toNestedIntervals a
