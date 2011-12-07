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

module RatCalc.Limits
    ( limitsToExactReal
    ) where

import Prelude hiding (exponent)

import RatCalc.Interval
import RatCalc.Number.ExactReal
import RatCalc.Number.SignedBinaryDigitStreamRepresentation as SBDSR
import RatCalc.Representation.SignedBinaryDigit
import RatCalc.Representation.SignedBinaryDigitStream

limitsToExactReal :: ToSBDSR a => [Interval a] -> ExactReal
limitsToExactReal is = fromSBDSR $ limitsToExactReal' (max lbe ube) r
  where
    (r @ ((Interval { lowerBound = lb, upperBound = ub }):_)) = map (mapInterval toSBDSR) is
    nlb = SBDSR.normalise lb
    nub = SBDSR.normalise ub
    lbe = exponent nlb
    ube = exponent nub

limitsToExactReal' :: Integer -> [Interval SBDSR] -> SBDSR
limitsToExactReal' e (i0:is) =
  SBDSR
    e
    (bananaBracketSBDSR e lb (limitsToExactReal' e is) ub)
  where
    lb = lowerBound i0
    ub = upperBound i0
limitsToExactReal' _ [] = error "limitsToExactReal'"

bananaBracketSBDSR :: Integer -> SBDSR -> SBDSR -> SBDSR -> SBDS
bananaBracketSBDSR e x y z  = bananaBracketSBDS (shiftN mx (e-ex)) (shiftN my (e-ey)) (shiftN mz (e-ez))
  where
      ex = exponent x
      ey = exponent y
      ez = exponent z
      mx = mantissa x
      my = mantissa y
      mz = mantissa z

bananaBracketSBDS :: SBDS -> SBDS -> SBDS -> SBDS
bananaBracketSBDS x y z
  | r_lo' > s_hi' = error "bananaBracketSBDS: r_lo > s_hi"
  | (r_lo' >= -4) && (s_hi' <= 4) =
      cons Z
        ( bananaBracketSBDS
            (rerepresentStarting Z x)
            (rerepresentStarting Z y)
            (rerepresentStarting Z z)
        )
  | r_lo' >= 0 =
      cons P
        ( bananaBracketSBDS
            (rerepresentStarting P x)
            (rerepresentStarting P y)
            (rerepresentStarting P z)
        )
  | s_hi' <= 0 =
      cons M
        ( bananaBracketSBDS
            (rerepresentStarting M x)
            (rerepresentStarting M y)
            (rerepresentStarting M z)
        )
  | r_hi' < s_lo' = y
  | otherwise =
      bananaBracketSBDS' 2 r' s' [a] [c] x' y z'
  where
    (a, x') = uncons x
    (c, z') = uncons z
    av = digitValue a
    cv = digitValue c
    (r' @ (r_lo', r_hi')) = (-4 + (av*4), 4 + (av*4))
    (s' @ (s_lo', s_hi')) = (-4 + (cv*4), 4 + (cv*4))



bananaBracketSBDS' :: Int -> (Int, Int) -> (Int, Int) -> [SBD] -> [SBD] -> SBDS -> SBDS -> SBDS -> SBDS
bananaBracketSBDS' sig (r_lo, r_hi) (s_lo, s_hi) outx outz x y z
  | sig == 0     = error "bananaBracketSBDS': sig == 0"
  | r_lo > s_hi  = error "bananaBracketSBDS': r_lo > s_hi"
  | abs r_lo > 8 = error "bananaBracketSBDS': |r_lo| > 8"
  | abs r_hi > 8 = error "bananaBracketSBDS': |r_hi| > 8"
  | abs s_lo > 8 = error "bananaBracketSBDS': |s_lo| > 8"
  | abs s_hi > 8 = error "bananaBracketSBDS': |s_hi| > 8"
  | (r_lo' >= -4) && (s_hi' <= 4) =
      cons Z
        ( bananaBracketSBDS
            ( rerepresentStarting Z (prefixDigits outx x))
            ( rerepresentStarting Z y)
            ( rerepresentStarting Z (prefixDigits outz z))
        )
  | r_lo' >= 0 =
      cons P
        ( bananaBracketSBDS
            ( rerepresentStarting P (prefixDigits outx x))
            ( rerepresentStarting P y)
            ( rerepresentStarting P (prefixDigits outz z))
        )
  | s_hi' <= 0 =
      cons M
        ( bananaBracketSBDS
            ( rerepresentStarting M (prefixDigits outx x))
            ( rerepresentStarting M y)
            ( rerepresentStarting M (prefixDigits outz z))
        )
  | r_hi' < s_lo' = y
  | otherwise = bananaBracketSBDS' (sig `div` 2) r' s' (outx ++ [a]) (outz ++ [c]) x' y z'
  where
    (a,x') = uncons x
    (c,z') = uncons z
    av = digitValue a
    cv = digitValue c
    (r'@(r_lo', r_hi')) = (r_lo + (av+1)*sig, r_hi + (av-1)*sig)
    (s'@(s_lo', s_hi')) = (s_lo + (cv+1)*sig, s_hi + (cv-1)*sig)

