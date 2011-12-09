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

module RatCalc.Symbolic.Expression where

import Data.Char
import Data.List as List
import Data.Map (Map)
import Data.Ratio
import Data.Set (Set)
import Text.Parsec
import Text.Parsec.Expr
import qualified Data.Map as Map
import qualified Data.Set as Set

data Expression =
      Number Integer
    | Symbol String
    | Application Expression [Expression]
    deriving (Eq)


-- TODO: Don't show unnecessary brackets
instance Show Expression where
    show (Number x)
        | x >= 0 = show x
        | otherwise = "(" ++ show x ++ ")" -- FIXME: This is necessary, because -12^4 = -(12^4), not (-12)^4
    show (Symbol s) = s
    show (Application o [e]) = "(" ++ show o ++ show e ++ ")"
    show (Application o e) = "(" ++ (concat $ List.intersperse (show o) (map show e)) ++ ")"

fromString s = runParser expressionParser () "" s 

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
             return (\l r -> Application (Symbol name) [l, r])
        ) assoc

prefixOperator name =
    Prefix
        ( do string name
             return (\r -> Application (Symbol name) [r])
        )

prefixNegate :: Monad m => Operator String () m Expression
prefixNegate =
    Prefix
        ( do string "-"
             return
                ( \r ->
                    case r of
                        Number n -> Number (-n)
                        e -> Application (Symbol "-") [e]
                )
        )

termParser = numberParser <|> parenthesisedParser expressionParser

parenthesisedParser f =
    do char '('
       e <- f
       char ')'
       return e

numberParser =
    do digits <- many1 digit
       let digits' = map (\x -> ord x - ord '0') digits
        in return $ Number $ digitsToNumber 10 0 digits'

digitsToNumber :: Integer -> Integer -> [Int] -> Integer
digitsToNumber base a (h:t) = digitsToNumber base (base*a + toInteger h) t
digitsToNumber _ a [] = a

