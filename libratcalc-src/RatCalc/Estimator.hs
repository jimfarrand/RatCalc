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

class Estimator a where
    toNestedIntervals :: a -> [(Rational, Rational)]

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

