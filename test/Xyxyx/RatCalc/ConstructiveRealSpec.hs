{-
-- RatCalc - An arbitrary precision numeric computation framework
-- Copyright © 2017 Jim Farrand
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

module Xyxyx.RatCalc.ConstructiveRealSpec where

import Test.Hspec
import Test.QuickCheck
import Xyxyx.RatCalc.ConstructiveReal

spec = do
    describe "CReal" $ do
        describe "approximate" $ do
            it "can approximate rationals to the correct Double" $ property $
                \r -> approximate (rational r) == fromRational r
        describe "+" $ do
            it "can add rationals so they approximate to the correct Double" $ property $
                \x y -> approximate ((rational x) + (rational y)) == fromRational (x+y)
            it "can add the negation of a rational to give a value that approximates to 0" $ property $
                \x -> approximate ((rational x) + (negate (rational x))) == 0
        describe "-" $ do
            it "can subtract rationals so they approximate to the correct Double" $ property $
                \x y -> approximate ((rational x) - (rational y)) == fromRational (x-y)
        describe "*" $ do
            it "can multiply rationals so they approximate to the correct Double" $ property $
                \x y -> approximate ((rational x) * (rational y)) == fromRational (x*y)
        describe "negate" $ do
            it "can negate rationals so they approximate to the correct Double" $ property $
                \x -> approximate (negate (rational x)) == fromRational (negate x)

