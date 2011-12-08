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

{- Quickcheck tests -}

module Test.RatCalc.QuickChecks where

import Data.Ratio
import RatCalc.Estimator
import RatCalc.Number.SignedBinaryDigitStreamRepresentation
import Test.QuickCheck

tolerance = 1%(2^256)

i :: Integer -> SBDSR
i = fromInteger

r :: Rational -> SBDSR
r = fromRational

prop_approxEqual x = approxEqual tolerance (r x) x

prop_approxEqualApprox x = approxEqualApprox tolerance (r x) (r x)

prop_approxNotEqual x y = x /= y ==> not (approxEqual tolerance (r x) y)

prop_approxNotEqualApprox x y = x /= y ==> not (approxEqualApprox tolerance (r x) (r y))

prop_add x y = approxEqual tolerance (r x + r y) (x+y)

prop_subtract x y = approxEqual tolerance (r x - r y) (x-y)

prop_multiply x y = approxEqual tolerance (r x * r y) (x*y)

prop_divide x y = y /= 0 ==> approxEqual tolerance (r x / r y) (x/y)

myCheck :: Testable prop => Int -> prop -> IO ()
myCheck count = quickCheckWith args
    where
        args = stdArgs { maxSuccess = count }

runTests count =
    do myCheck count prop_approxEqual
       myCheck count prop_approxEqualApprox
       myCheck count prop_approxNotEqual
       myCheck count prop_approxNotEqualApprox
       myCheck count prop_add
       myCheck count prop_subtract
       myCheck count prop_multiply
       myCheck count prop_divide

main = runTests 1000
