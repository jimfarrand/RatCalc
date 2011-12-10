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


module RatCalc.Number.SignedBinaryDigitStreamRepresentation.Tests
    ( unitTests, quickChecks
    ) where

import Data.Ratio
import RatCalc.Estimator
import RatCalc.Number.SignedBinaryDigitStreamRepresentation
import RatCalc.Test.QuickCheck.Utils
import Test.HUnit
import Test.QuickCheck

unitTests =
    TestList
        [
        ]

quickChecks =
    [ QC checkApproxEqual
    , QC checkApproxEqualApprox
    , QC checkApproxNotEqual
    , QC checkApproxNotEqualApprox
    , QC checkAdd
    , QC checkSubtract
    , QC checkMultiply
    , QC checkDivide
    ]

checkApproxEqual x = approxEqual tolerance (r x) x

checkApproxEqualApprox x = approxEqualApprox tolerance (r x) (r x)

checkApproxNotEqual x y = x /= y ==> not (approxEqual tolerance (r x) y)

checkApproxNotEqualApprox x y = x /= y ==> not (approxEqualApprox tolerance (r x) (r y))

checkAdd x y = approxEqual tolerance (r x + r y) (x+y)

checkSubtract x y = approxEqual tolerance (r x - r y) (x-y)

checkMultiply x y = approxEqual tolerance (r x * r y) (x*y)

checkDivide x y = y /= 0 ==> approxEqual tolerance (r x / r y) (x/y)

tolerance = 1%(2^256)

r :: Rational -> SBDSR
r = fromRational
