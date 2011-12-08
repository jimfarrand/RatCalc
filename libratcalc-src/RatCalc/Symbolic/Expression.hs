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

import Text.ParserCombinators.ReadP as ReadP
import Data.Char
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Expression =
      Number Rational
    | Symbol String
    | Application Expression [Expression]
    deriving (Eq, Show)


data Operator =
    Operator
        { operatorSymbol :: String
        , operatorAssociativity :: Associativity
        , operatorPrecedence :: Int
        }
    deriving (Eq, Ord, Show)

data Associativity = LeftAssociative | NonAssociative | RightAssociative
    deriving (Eq, Ord, Show)

fromString :: [Operator] -> String -> Expression
fromString operators s =
    case readP_to_S (parseExpression operators operators) s of
        [(e, "")] -> e
        _ -> error $ "parseExpression: parse failed: " ++ s

defaultOperators = -- operators
    [ Operator
        { operatorSymbol = "^"
        , operatorAssociativity = RightAssociative
        , operatorPrecedence = 8
        }
    , Operator
        { operatorSymbol = "*"
        , operatorAssociativity = LeftAssociative
        , operatorPrecedence = 7
        }
    , Operator
        { operatorSymbol = "/"
        , operatorAssociativity = LeftAssociative
        , operatorPrecedence = 7
        }
    , Operator
        { operatorSymbol = "+"
        , operatorAssociativity = RightAssociative
        , operatorPrecedence = 6
        }
    , Operator
        { operatorSymbol = "-"
        , operatorAssociativity = RightAssociative
        , operatorPrecedence = 6
        }
    ]

parseExpression allOperators currentOperators =
    do leftTerm <- parseTerm
       parseExpression' allOperators currentOperators leftTerm

parseExpression' allOperators currentOperators leftTerm =
    parseOperationExpression allOperators currentOperators leftTerm <++ return leftTerm

parseOperationExpression allOperators currentOperators leftTerm =
    do o <- parseOperation currentOperators
       case operatorAssociativity o of
         LeftAssociative  -> do rightTerm <- parseExpression allOperators (filter (\o' -> operatorPrecedence o' > operatorPrecedence o) currentOperators)
                                parseExpression' allOperators allOperators $ Application (Symbol (operatorSymbol o)) [leftTerm, rightTerm]

         RightAssociative -> do rightTerm <- parseExpression allOperators (filter (\o' -> operatorPrecedence o' >= operatorPrecedence o) currentOperators)
                                parseExpression' allOperators allOperators $ Application (Symbol (operatorSymbol o)) [leftTerm, rightTerm]

parseOperation operators =
    choice (map parseOperation' operators)

parseOperation' o =
    do string (operatorSymbol o)
       return o


parseOperator os = choice (map parseOperator' os)
    where
        parseOperator' o = string (operatorSymbol o)

parseBracketedExpression allOperators =
    do char '('
       e <- parseExpression allOperators allOperators
       char ')'
       return e

parseTerm = parseNumber

parseNumber =
    do i <- parseInteger
       return $ Number $ fromInteger i

parseInteger :: ReadP Integer
parseInteger =
    do digits <- many1 parseDigit
       return $ combineDigits 0 digits
    where
        combineDigits :: Integer -> [Int] -> Integer
        combineDigits a [] = a
        combineDigits a (h:t) = combineDigits (10*a + toInteger h) t

parseDigit :: ReadP Int
parseDigit =
    do x <- satisfy (\x -> x >= '0' && x <= '9')
       return $ ord x - ord '0'

