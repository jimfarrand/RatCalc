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


module RatCalc.Data.GenericTree.Tests where

import Test.HUnit
import RatCalc.Data.GenericTree.Zipper.Tests as Zipper

unitTests :: Test
unitTests =
    TestList
        [ TestLabel "Zipper" Zipper.unitTests
        ]

quickChecks =
    Zipper.quickChecks
