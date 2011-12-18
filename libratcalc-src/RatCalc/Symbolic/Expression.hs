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

{- Expressions representation, parsing and printing -}

{-# LANGUAGE TypeSynonymInstances, FlexibleContexts #-}

module RatCalc.Symbolic.Expression where

import Data.Char
import Data.List as List
import RatCalc.Data.GenericTree hiding (map)
import RatCalc.Number.SignedBinaryDigitStreamRepresentation
import Text.Parsec
import Text.Parsec.Expr

type Expression = GenericTree Term Function

data Term = Integer Integer | Real SBDSR | Symbol Symbol
    deriving (Eq, Ord, Show)

data Function =
    Function
        { functionName  :: FunctionName
        , infixOperator :: Bool
        }
    deriving (Eq, Ord, Show)

type FunctionName = String
type Symbol = String

-- This could be combined with the version in pretty print (with identity function as effects)
showExpression (Leaf (Integer x))
    | x >= 0 = show x
    | otherwise = "(" ++ show x ++ ")" -- FIXME: This is necessary, because -12^4 = -(12^4), not (-12)^4
showExpression (Leaf (Real x))
    | x >= 0 = show x
    | otherwise = "(" ++ show x ++ ")" -- FIXME: This is necessary, because -12^4 = -(12^4), not (-12)^4
showExpression (Leaf (Symbol s)) = s
showExpression (Branch f e)
    | infixOperator f = "(" ++ (concat $ List.intersperse (functionName f) (map showExpression e)) ++ ")"
    | otherwise = functionName f ++ "(" ++ (concat $ List.intersperse ", " (map showExpression e)) ++ ")"

fromString s = runParser parser () "" s 
    where
        parser = do
            e <- expressionParser
            eof
            return e

fromString' s =
    case fromString s of
        Right x -> x
        Left e -> error $ "parse failed \"" ++ s ++ "\": " ++ show e

expressionParser = buildExpressionParser table termParser

table :: Monad m => OperatorTable String () m Expression
table =
    [ [ binaryOperator "^" AssocRight ]
    , [ binaryOperator "*" AssocLeft, binaryOperator "/" AssocLeft, prefixNegate ]
    , [ binaryOperator "+" AssocRight, binaryOperator "-" AssocRight]
    ]

binaryOperator name assoc =
    Infix
        ( do string name
             spaces
             return (\l r -> Branch (Function name True) [l, r])
        ) assoc

prefixOperator name =
    Prefix
        ( do string name
             spaces
             return (\r -> Branch (Function name False) [r])
        )

prefixNegate :: Monad m => Operator String () m Expression
prefixNegate =
    Prefix
        ( do string "-"
             spaces
             return
                ( \r ->
                    case r of
                        Leaf (Integer n) -> Leaf (Integer (-n))
                        e -> Branch (Function "-" False) [e]
                )
        )

termParser = numberParser <|> functionOrSymbolParser <|> parenthesisedParser expressionParser

parenthesisedParser f =
    do char '('
       spaces
       e <- f
       char ')'
       spaces
       return e

functionOrSymbolParser =
    do functionName <- many1 letter
       spaces
       functionParser functionName <|> (return (Leaf (Symbol functionName)))

functionParser functionName =
    do functionArgs <- parenthesisedParser (functionArgumentParser [])
       spaces
       return (Branch (Function functionName False) functionArgs)

functionArgumentParser a =
    do e <- expressionParser
       ( char ',' >> spaces >> functionArgumentParser (e:a)) <|> ( return (reverse (e:a)))

numberParser =
    do digits <- many1 digit
       spaces
       let digits' = map (\x -> ord x - ord '0') digits
        in return $ Leaf $ Integer $ digitsToInteger 10 0 digits'

digitsToInteger :: Integer -> Integer -> [Int] -> Integer
digitsToInteger base a (h:t) = digitsToInteger base (base*a + toInteger h) t
digitsToInteger _ a [] = a

