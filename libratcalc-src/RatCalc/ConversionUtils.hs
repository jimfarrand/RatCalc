{-
-- RatCalc - An infinite precision numeric computation framework
-- Copyright (C) 2010, 2011 Jim Farrand
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


module RatCalc.ConversionUtils where

import Data.Char
import Data.List
import Data.Ratio
import qualified Data.Set as Set


integerToBits n
    | n >= 0 = reverse (integerToBits' n)
    where
        integerToBits' n
            | n == 0 = []
            | odd n = True : integerToBits' (n `div` 2)
            | otherwise = False : integerToBits' (n `div` 2)

-- |Convert a single digit in the range 0-35 inclusive to a Char
digitFromInt :: Integral a => a -> Char
digitFromInt n
  | n < 0 = error "showDigit"
  | n < 10 = chr (ord '0' + fromIntegral n)
  | n < 36 = chr (ord 'a' + fromIntegral (n-10))
  | otherwise = error "showDigit"

digitsFromInts :: Integral a => [a] -> String
digitsFromInts = map digitFromInt

-- |Convert an integral to a string in the given base
showIntegerInBase :: (Integral a) => a -> a -> String
showIntegerInBase base n = reverse (showIntegerInBase' n)
  where
    showIntegerInBase' n
      | n == 0 = []
      | otherwise = digitFromInt digit : showIntegerInBase' n'
      where
        digit = n `mod` base
        n' = (n - digit) `div` base
        
-- |Convert a Rational to a string in the given base, in fractional representation
showRationalInBase :: (Integral a) => a -> Ratio a -> String
showRationalInBase base r =
  showIntegerInBase base (numerator r) ++ "/" ++
  showIntegerInBase base (denominator r)


showRationalAsNonRecurringInBase :: Integer -> Int -> Rational -> String
showRationalAsNonRecurringInBase base p r
  | r >= 0 = showIntegerInBase base whole ++ "." ++ take p (digitsFromInts digits) -- FIXME: Rounding?
  | otherwise =
      '-' : showRationalAsNonRecurringInBase base p (negate r)
  where
    n = numerator r
    d = denominator r
    whole = n `Prelude.div` d
    remainder = n - (whole * d)
    digits = findDigits (toInteger base) remainder d

    findDigits :: (Integral a) => Integer -> a -> a -> [a]
    findDigits base remainder divisor = digit:findDigits base remainder' divisor
      where
        dividend = remainder * fromInteger base
        digit = dividend `div` divisor
        remainder' = dividend - (digit * divisor)



-- |Show a rational as a n-imal (eg decimal) recurring fraction.
-- Eg (1/30) -> 0.0(3)
showRationalAsRecurringInBase :: Integral a => a -> Ratio a -> String
showRationalAsRecurringInBase base r = compressLeadingZeros 100 base str
  where
    n = numerator r
    d = denominator r
    whole = n `Prelude.div` d    
    remainder = n - (whole * d)
    (nonRecurring, recurring) = findRecurringDigits base remainder d
    recurringPart =
      if recurring == [0]
        then ""
        else "(" ++ digitsFromInts recurring ++ ")"
    str = show whole ++ ( if null nonRecurring && null recurringPart then "" else "." ++ digitsFromInts nonRecurring ++ recurringPart)

findRecurringDigits :: Integral a => a -> a -> a -> ([a], [a])
findRecurringDigits base remainder divisor = (map fst nonRepeatingPart, map fst repeatingPart)
  where
    digitsAndRemainders = findDigitsAndRemainders base remainder divisor
    recurrenceRemainder = findRepeatedValue $ map snd digitsAndRemainders
    notSplitPoint = (recurrenceRemainder /= ) . snd
    (nonRepeatingPart, rest) = span notSplitPoint digitsAndRemainders
    repeatingPart = head rest : takeWhile notSplitPoint (tail rest)

    findDigitsAndRemainders :: (Integral a) => a -> a -> a -> [(a, a)]
    findDigitsAndRemainders base remainder divisor = (digit, remainder):findDigitsAndRemainders base remainder' divisor
      where
        dividend = remainder * base
        digit = dividend `div` divisor
        remainder' = dividend - (digit * divisor)

findRepeatedValue :: Ord a => [a] -> a
findRepeatedValue = findRepeatedValue' Set.empty
  where
    findRepeatedValue' _ [] = error "findRepeatedValue: no repetitions"
    findRepeatedValue' seen (r:rs)
      | Set.member r seen = r
      | otherwise = findRepeatedValue' (Set.insert r seen) rs
                    
-- Replace leading zeros before the point with a multiple of a power of the base at the end.
compressLeadingZeros maxZeros base ('0':'.':t)
  | count >= maxZeros = '0':'.':(genericDrop count t ++ "*" ++ show base ++ "^(" ++ show (negate count) ++ ")")
  where
    count = genericLength (takeWhile (=='0') t)
compressLeadingZeros _ _ l = l
