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
    describe "Constructive Reals" $ do
        it "rationals approximate to correct value" $ property $
            \r -> approximate (rational r) == fromRational r
        it "added rationals approximate to correct value" $ property $
            \x y -> approximate ((rational x) + (rational y)) == fromRational (x+y)
        it "negated rationals approximate to correct value" $ property $
            \x -> approximate (negate (rational x)) == fromRational (negate x)
        it "adding negation approximates to zero" $ property $
            \x -> approximate ((rational x) + (negate (rational x))) == 0

