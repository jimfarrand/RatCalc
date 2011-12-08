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

module Test.RatCalc.UnitTests where

import Test.HUnit
import System.Exit
import qualified Test.RatCalc.Symbolic.Expression as Expression

tests =
    TestList
        [ TestLabel "Test.RacCalc.Symbolic.Expression" Expression.tests
        ]

runTests = runTestTT tests

main = do count <- runTests
          if errors count > 0 then
            exitWith (ExitFailure 1)
          else if failures count > 0 then
            exitWith (ExitFailure 2)
          else
            return ()
