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

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module RatCalc.Symbolic.Expression.Tests where

import Control.Monad
import Test.HUnit
import Test.QuickCheck
import RatCalc.Symbolic.Expression
import RatCalc.Test.QuickCheck.Utils
import RatCalc.Data.GenericTree

unitTests :: Test
unitTests =
    TestList
        [ TestLabel "1" $ testParse "1" (Leaf (Number 1))
        , TestLabel "1+2" $ testParse "1+2" (Branch (Function { functionName = "+", infixOperator = True}) [Leaf (Number 1), Leaf (Number 2)])
        , TestLabel "1+2+3" $ testParse "1+2+3" (Branch (Function { functionName = "+", infixOperator = True}) [Leaf (Number 1), Branch (Function { functionName = "+", infixOperator = True}) [Leaf (Number 2), Leaf (Number 3)]])
        , TestLabel "1+2*3" $ testParse "1+2*3" (Branch (Function { functionName = "+", infixOperator = True}) [Leaf (Number 1), Branch (Function { functionName = "*", infixOperator = True}) [Leaf (Number 2), Leaf (Number 3)]])
        , TestLabel "1*2+3" $ testParse "1*2+3" (Branch (Function { functionName = "+", infixOperator = True}) [Branch (Function { functionName = "*", infixOperator = True}) [Leaf (Number 1), Leaf (Number 2)], Leaf (Number 3)])
        , TestLabel "1*2*3" $ testParse "1*2*3" (Branch (Function { functionName = "*", infixOperator = True}) [Branch (Function { functionName = "*", infixOperator = True}) [Leaf (Number 1), Leaf (Number 2)], Leaf (Number 3)])
        , TestLabel "1*2^3^4/5" $ testParse "1*2^3^4/5" (Branch (Function { functionName = "/", infixOperator = True}) [Branch (Function { functionName = "*", infixOperator = True}) [Leaf (Number 1), Branch (Function { functionName = "^", infixOperator = True}) [Leaf (Number 2), Branch (Function { functionName = "^", infixOperator = True}) [Leaf (Number 3), Leaf (Number 4)]]], Leaf (Number 5)])
        ]

testParse :: String -> Expression -> Test
testParse s r = TestCase (assertEqual ("parsed expression: " ++ show s) r (fromString' s))


quickChecks :: [QC]
quickChecks =
    [ QC checkParser
    ]

instance Arbitrary Term where
    arbitrary = oneof [ liftM Number arbitrary ] --, liftM Symbol arbitrary ]

instance Arbitrary Expression where
    arbitrary = sized expression'
        where
            expression' 0 = oneof [liftM Leaf arbitrary]
            expression' size =
                oneof
                    [ liftM Leaf arbitrary
                    , liftM2
                        Branch
                        (oneof
                            [ return (Function { functionName = "+", infixOperator = True})
                            , return (Function { functionName = "-", infixOperator = True})
                            , return (Function { functionName = "*", infixOperator = True})
                            , return (Function { functionName = "/", infixOperator = True})
                            , return (Function { functionName = "^", infixOperator = True})
                            ])
                        args
                    ]
                where
                    subExpression = expression' (size `div` 2)
                    args = do l <- subExpression
                              r <- subExpression
                              return [l, r]

checkParser :: Expression -> Bool
checkParser x = x == fromString' (showExpression x)