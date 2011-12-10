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

{- Unit tests for everything -}

{-# OPTIONS_GHC -XDoAndIfThenElse #-}

module RatCalc.Tests where

import Test.HUnit
import System.Exit
import qualified RatCalc.Symbolic.Tests as Symbolic
import qualified RatCalc.Data.Tests as Data
import qualified RatCalc.Number.Tests as Number

unitTests =
    TestList
        [ TestLabel "Symbolic" Symbolic.unitTests
        , TestLabel "Data" Data.unitTests
        , TestLabel "Number" Number.unitTests
        ]

quickChecks =
    concat
        [ Symbolic.quickChecks
        , Data.quickChecks
        , Number.quickChecks
        ]

