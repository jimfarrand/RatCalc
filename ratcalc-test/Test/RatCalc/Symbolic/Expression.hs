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

{- Tests for expressions -}

module Test.RatCalc.Symbolic.Expression where

import Test.HUnit
import RatCalc.Symbolic.Expression

testParse s r = TestCase (assertEqual ("parsed expression: " ++ show s) (fromString defaultOperators s) r)

tests =
    TestList
        [ TestLabel "1" $ testParse "1" (Number 1)
        , TestLabel "1+2" $ testParse "1+2" (Application (Symbol "+") [Number 1, Number 2])
        , TestLabel "1+2+3" $ testParse "1+2+3" (Application (Symbol "+") [Number 1, Application (Symbol "+") [Number 2, Number 3]])
        , TestLabel "1+2*3" $ testParse "1+2*3" (Application (Symbol "+") [Number 1, Application (Symbol "*") [Number 2, Number 3]])
        , TestLabel "1*2+3" $ testParse "1*2+3" (Application (Symbol "+") [Application (Symbol "*") [Number 1, Number 2], Number 3])
        , TestLabel "1*2*3" $ testParse "1*2*3" (Application (Symbol "*") [Application (Symbol "*") [Number 1, Number 2], Number 3])
        ]
