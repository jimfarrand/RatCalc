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


{-
 - Estimators are able to estimate values to arbitrary precision, by
 - calculating increasingly tight intervals around those that value.
 -}

{-# OPTIONS_GHC -XNoMonomorphismRestriction #-}

module RatCalc.Estimator where

import Data.Ratio
import Numeric
import RatCalc.ConversionUtils
import RatCalc.Stepped

class Estimator a where
    toNestedIntervals :: a -> [(Rational, Rational)]

instance Estimator Integer where
    toNestedIntervals a = [(toRational a, toRational a)]

instance (Integral a, Real a) => Estimator (Ratio a) where
    toNestedIntervals a = [(toRational a, toRational a)]

infix 4 ~=, =~, ~=~

(~=) = approxEqual precision
(=~) = flip (~=)
(~=~) = approxEqualApprox precision

precision = 1%(2^256)

approxEqual :: Estimator a => Rational -> a -> Rational -> Bool
approxEqual precision a b = approxEqual' precision (toNestedIntervals a) b

approxEqual' _ [] _ = error "approxEqual: no interval"
approxEqual' _ [(l, h)] v = (l <= v && v <= h)
approxEqual' precision ((l, h):t) v =
    if l <= v && v <= h then
        if (h-l) <= precision then
            True
        else
            approxEqual' precision t v
    else
        False

approxEqualApprox :: (Estimator a, Estimator b) => Rational -> a -> b -> Bool
approxEqualApprox precision a b = approxEqualApprox' precision (toNestedIntervals a) (toNestedIntervals b)

approxEqualApprox' _ [] _ = error "approxEqualApprox: no interval"
approxEqualApprox' _ _ [] = error "approxEqualApprox: no interval"
approxEqualApprox' _ [(l0, h0)] [(l1, h1)] = overlap l0 h0 l1 h1
approxEqualApprox' precision (i0 @ [(l0, h0)]) ((l1, h1):t0) =
    if overlap l0 h0 l1 h1 then
        if h1 - l1 <= precision then
            True
        else
            approxEqualApprox' precision i0 t0
    else
        False
approxEqualApprox' precision ((l0, h0):t0) (i1 @ [(l1, h1)]) =
    if overlap l0 h0 l1 h1 then
        if h0 - l0 <= precision then
            True
        else
            approxEqualApprox' precision t0 i1
    else
        False
approxEqualApprox' precision (i0 @ ((l0, h0):t0)) (i1 @ ((l1, h1):t1)) =
    if overlap l0 h0 l1 h1 then
        if d0 <= precision && d1 <= precision then
            True
        else
            case compare d0 d1 of
                LT -> approxEqualApprox' precision i0 t1
                GT -> approxEqualApprox' precision t0 i1
                EQ -> approxEqualApprox' precision t0 t1
    else
        False
    where
        d0 = h0 - l0
        d1 = h1 - l1

unsafeApproxCompareApprox :: (Estimator a, Estimator b) => a -> b -> Ordering
unsafeApproxCompareApprox a b = unsafeApproxCompareApprox' (toNestedIntervals a) (toNestedIntervals b)
    where
        unsafeApproxCompareApprox' (i0 @ ((l0,h0):t0)) (i1 @ ((l1,h1):t1))
            | overlap l0 h0 l1 h1 =
                case compare d0 d1 of
                    LT -> unsafeApproxCompareApprox' i0 t1
                    GT -> unsafeApproxCompareApprox' t0 i1
                    EQ ->
                        if d0 == 0 then
                            EQ -- Unlikely!
                        else
                            unsafeApproxCompareApprox' t0 t1
            | h0 < l1 = LT
            | h1 < l0 = GT
            | otherwise = error "unsafeApproxCompareApprox"
            where
                d0 = h0 - l0
                d1 = h1 - l1

overlap l0 h0 l1 h1 = (l0 <= l1 && l1 <= h0) || (l1 <= l0 && l0 <= h1)

unsafeDigits :: Estimator a => Rational -> Rational -> a -> [Int]
unsafeDigits base current e = unsafeDigits' 0 current (toNestedIntervals e)
    where
        unsafeDigits' soFar digit ((l,h):t) =
            case findDigit l h 0 of
                Nothing -> unsafeDigits' soFar digit t
                Just d -> d : unsafeDigits' (soFar+(digit * fromInteger (toInteger d))) (digit/base) t
            where
                findDigit :: Rational -> Rational -> Int -> Maybe Int
                findDigit l h d
                    | l > dl && h < dh = Just d
                    | l > dh = findDigit l h (d+1)
                    | otherwise = Nothing
                    where
                        dl = soFar + (fromInteger (toInteger d)*digit)
                        dh = dl + digit


type Digit = Int
type Exponent = Int

-- Show an estimator as a n-imal (eg decimal number)
-- Digits which follow a "~" are approximate - they may actually be 1 lower than show.
-- These digits may be deleted using backspaces later in the output if they are
-- found to be wrong
-- Slightly arbitrarily, we round uncertain numbers up, so that we prefer
-- 1.00... to 0.99...
showEstimatorDigits base maxDigits v = showDigits' 0 0 0 (findDigits base v)
    where
        -- digits is the number of digits output
        -- unsureDigits is the number of digits output that we aren't sure about
        -- unsureCharacters is the number of characters we aren't sure about (including digits, the twiddle and point)
        showDigits' digits unsureDigits unsureChars ((certain, exponent, d):t) =
            if maxDigits - digits == 0 then
                ""
            else concat
                [ if unsureChars == 0 then
                    if certain then
                      ""
                    else
                      "~"
                  else
                    if certain then
                      concat
                        [ take unsureChars (repeat '\b')
                        , take unsureChars (repeat ' ')
                        , take unsureChars (repeat '\b')
                        ]
                    else
                      ""
                , if certain then
                    show d
                  else
                    show ((d+1) `mod` base)
                , if point then
                    "."
                  else
                    ""
                , showDigits' digits' unsureDigits' unsureChars' t
                ]
            where
                point = exponent == 0
                unsureChars' =
                    if certain then -- Now certain about everything
                        0
                    else if unsureChars == 0 then
                        if point then -- Uncertain about the twiddle, point, and digit
                            3
                        else -- Uncertain about the twiddle and digit
                            2
                    else
                        if point then
                            unsureChars+2 -- Uncertain about the previous characters, the point and the digit
                        else
                            unsureChars+1 -- Uncertain about the previous characters and the digit
                unsureDigits' =
                    if certain then -- Now certain about everything
                        0
                    else
                        unsureDigits+1 -- Uncertain about the previous digits and the current digit
                digits' =
                    if certain then
                        digits-unsureDigits+1 -- There were some digits, we lost all the uncertain ones, and added a new one
                    else
                        digits+1 -- There were some digits, we added one
data State =
    State
        { stateExponent      :: Exponent
        , stateDigit         :: Digit
        , stateDigitBottom   :: Rational
        , stateDigitTop      :: Rational
        , stateDigitSize     :: Rational
        }


-- Find the digits of a number.  Returns a list of triples (certain, exponent, digit)
--   where certain is True iff this digit is definitely correct, False if it may be off by one
-- Note that the value of the exponent is not necessarily monoticially
-- decreasing (when certain digits follow uncertain ones)
findDigits :: Estimator a => Int -> a -> [(Bool, Exponent, Digit)]
findDigits base v = digits base' state state es'
    where
        base' = toRational base
        (exponent, digitSize, es') = findExponent base' 0 1 (toNestedIntervals v)
        state =
            State
                { stateExponent = exponent
                , stateDigit = 0
                , stateDigitBottom = 0
                , stateDigitSize = digitSize
                , stateDigitTop = digitSize
                }

digits base certainState uncertainState (es@((l,h):_))
    | l < 0 = error "FIXME: I can't yet print numbers that may be negative"
    | l > digitTop =
        let state =
                certainState
                    { stateDigit         = digit + 1
                    , stateDigitBottom   = digitTop
                    , stateDigitTop      = digitTop'
                    }
         in digits base state state es
    | h < digitTop =
        let state =
                certainState
                    { stateDigit = 0
                    , stateExponent = exponent-1
                    , stateDigitTop = digitBottom + digitSize'
                    , stateDigitSize = digitSize'
                    }
         in (True, exponent, digit) : digits base state state es
    | otherwise = digits' base certainState uncertainState es
    where
        digit        = stateDigit        certainState
        digitBottom  = stateDigitBottom  certainState
        digitTop     = stateDigitTop     certainState
        digitSize    = stateDigitSize    certainState
        exponent     = stateExponent     certainState
        digitSize'   = digitSize / base
        digitTop'    = digitTop + digitSize

digits' base certainState uncertainState (es@((l,h):es'))
    | l > digitTop =
        let state =
                uncertainState
                    { stateDigit         = digit + 1
                    , stateDigitBottom   = digitTop
                    , stateDigitTop      = digitTop'
                    }
         in digits base certainState state es
    | h < digitTop' =
        let state =
                certainState
                    { stateDigit = 0
                    , stateExponent = exponent-1
                    , stateDigitTop = digitBottom + digitSize'
                    , stateDigitSize = digitSize'
                    }
         in (False, exponent, digit) : digits base certainState state es
    | otherwise = digits base certainState uncertainState es'
    where
        digit        = stateDigit        uncertainState
        digitBottom  = stateDigitBottom  uncertainState
        digitTop     = stateDigitTop     uncertainState
        digitSize    = stateDigitSize    uncertainState
        exponent     = stateExponent     uncertainState
        digitSize'   = digitSize / base
        digitTop'    = digitTop + digitSize

{-
digits :: Estimator a => Rational -> a -> [(Rational, Maybe Int)]
digits base v = digits' 0 digitSize 0 exponent es
    where
        (exponent, digitSize, es) = findExponent base 0 1 (toNestedIntervals v)

        digits' :: Rational -> Rational -> Int -> Integer -> [(Rational, Rational)] -> [(Rational, Maybe Int)]
        digits' digitBottom digitSize digit exponent (es@((l,h):es'))
            | l > digitTop = digits' digitTop digitSize (digit+1) exponent es
            | h < digitTop = (d, Just digit) : digits' digitBottom (digitSize/base) 0 (exponent-1) es
            | otherwise = (d, Nothing) : digits' digitBottom digitSize digit exponent es'
            where
                digitTop = digitBottom + digitSize
                d = h - l
-}

findExponent base exponent digit (e @ ((l,h):t))
    | l > digit' = findExponent base (exponent+1) digit' e
    | h < -digit' = findExponent base (exponent+1) digit' e
    | l >= -digit' && h <= digit' = (exponent, digit, e)
    | otherwise = findExponent base exponent digit t
    where
        digit' = digit*base


{-
showRationalApproximations base v =
    map
        (\(r,e) -> showRationalAsNonRecurringInBase base (max (-e) 0) r ++ " +/- " ++ show base ++ "^" ++ show e)
        (findRationalApproximations (fromInteger base) exponent digitSize is)
    where
        (exponent,digitSize, is) = findExponent (fromInteger base) 0 1 (toNestedIntervals v)

findRationalApproximations :: Rational -> Int -> Rational -> [(Rational, Rational)] -> [(Rational, Int)]
findRationalApproximations base exponent digitSize (es@((l,h):es'))
    | e-digitSize <= l && e+digitSize >= h = (e,exponent) : findRationalApproximations base (exponent-1) (digitSize/base) es
    | otherwise = findRationalApproximations base exponent digitSize es'

    where
        e = approxRational ((l+h)/2) ((h-l)/2)
-}


{-
expansion base digitBottom digitSize exponent digit (es@((l,h):es'))
    | l > digitTop = expansion base digitTop digitSize exponent (digit+1) es
    | h < digitTop = Certain exponent digit (expansion base digitBottom (digitSize / base) (exponent+1) 0)
    | otherwise = uncertainExpansion base certainBreak certainDigitSize certainExponent certainDigit 
    where
        digitTop = digitBottom + digitSize

uncertainExpansion base certainBreak certainDigitSize certainExponent certainDigit digitBottom digitSize exponent digit (es@((l,h):es')) =
    | h < digitBreak = Certain certainExponent certainDigit (expansion base 

-}

{-

Uncertainty is always:
    (x)99999
  (1+x)00000

-}

{-
uncertainExpansion base digitBottom digitSize digit l h
    | l > digitTop = uncertainExpansion base digitTop digitSize exponent (digit+1) l h
    | h < digitTop' = digit : uncertainExpansion base digitBottom (digitSize / base) (exponent+1) 
-}



{-
findDigitApproximations' base digitSize (e @ ((l,h):t))
    | h - l <= digitSize = (l,digitSize) : findDigitApproximations' base (digitSize / base) t
    | otherwise = findDigitApproximations' base digitSize t
-}


{-
findNext :: Rational -> Rational -> Rational -> Int -> [(Rational, Rational)] -> Stepped [(Rational, Rational)] Int
findNext base digitBottom digitSize digit (es@((l,h):t))
    | l > digitTop  = findNext base digitTop digitSize (digit+1) es
    | h < digitTop  = Done es digit
    | otherwise = Tick es (findNext base digitBottom digitSize digit t)
    where
        digitTop = digitBottom + digitSize

-}

{-
data DigitExpansion = Certain Digit DigitExpansion | Uncertain [(Digit, Digit)] DigitExpansion

type Digit = Int


type Expansion = StopList Digit (StopList (Digit, Digit) ())

collapseExpansion :: Int -> [(Int, Int)] -> Expansion
collapseExpansion _ [] = End (End ())
collapseExpansion base (ds@((l,h):ds'))
    | l == h = Cons l (collapseExpansion base ds')
    | otherwise = End (collapseExpansion' ds')

    where
        collapseExpansion' (ds@((l,h):ds'))
            | (l+1 `mod` base) == h = Cons (l,h) (collapseExpansion' ds')
        collapseExpansion' _ = End ()

findExpansions base bottom digitSize = map (\(l, h) -> zip (findDigits base bottom digitSize l) (findDigits base bottom digitSize h))

-- Eg 0.12345 -> [0,1,2,3,4,5]
findDigits :: Rational -> Rational -> Rational -> Rational -> [Int]
findDigits base bottom digitSize l = digit : findDigits base (bottom+(toRational digit*digitSize)) (digitSize / base) l
    where
        digit = floor ((l-bottom) / digitSize)

-}
