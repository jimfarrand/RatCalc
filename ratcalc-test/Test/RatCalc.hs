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

{- Bring together all tests into an easily runnable suite. -}

{-# OPTIONS_GHC -XDoAndIfThenElse #-}

import qualified Test.RatCalc.QuickChecks as QuickChecks
import qualified Test.RatCalc.UnitTests as UnitTests
import Test.HUnit
import System.Exit

main =
    do putStrLn "Running unit tests..."
       count <- UnitTests.runTests
       if errors count > 0 then
         exitWith (ExitFailure 1)
       else if failures count > 0 then
         exitWith (ExitFailure 2)
       else
         return ()
       putStrLn "Running quickchecks..."
       QuickChecks.runTests 100

