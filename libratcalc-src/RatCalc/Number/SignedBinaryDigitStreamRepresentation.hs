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

{-# OPTIONS_GHC -XFlexibleInstances #-}

module RatCalc.Number.SignedBinaryDigitStreamRepresentation where

import Control.Parallel
import Data.Ratio
import Data.Ratio as Ratio
import RatCalc.BuildConfig
import RatCalc.ConversionUtils
import RatCalc.Arithmetic
import RatCalc.Estimator
import RatCalc.Representation.DyadicRational as DR
import RatCalc.Representation.DyadicRationalStream as DRS
import RatCalc.Representation.SignedBinaryDigit as SBD
import RatCalc.Representation.SignedBinaryDigitStream hiding (normalise, showBits, withParallelLookahead, withParallelLookahead')

import qualified RatCalc.Representation.SignedBinaryDigitStream as SBDS

data SBDSR = SBDSR Integer SBDS

exponent (SBDSR e _) = e
mantissa (SBDSR _ m) = m

withParallelLookahead s =
    if multiprocessor then
        withParallelLookahead' s
    else
        s
withParallelLookahead' (SBDSR e m) = {- par e -} (SBDSR e (SBDS.withParallelLookahead m))

instance Show SBDSR where
    show = showBits 32

showBits n (SBDSR exponent mantissa) = concat [ "2^", show exponent, "*", SBDS.showBits n mantissa]

instance Eq SBDSR where

instance Ord SBDSR where
    compare = unsafeApproxCompareApprox

instance Estimator SBDSR where
    toNestedIntervals (SBDSR exponent mantissa)
        | exponent > 0 = scale ((scalingFactor exponent)%1) mantissaIntervals
        | exponent < 0 = scale (1%(scalingFactor (-exponent))) mantissaIntervals
        | otherwise = mantissaIntervals
        where
            mantissaIntervals = toNestedIntervals mantissa
            scale f = map (\(x,y) -> (f*x, f*y))

class ToSBDSR a where
    toSBDSR :: a -> SBDSR

instance ToSBDSR SBDSR where
    toSBDSR = id

instance ToSBDSR Integer where
    toSBDSR = fromInteger

instance (Integral a, ToSBDSR a) => ToSBDSR (Ratio a) where
    toSBDSR x = toSBDSR (Ratio.numerator x) / toSBDSR (Ratio.denominator x)

scalingFactor :: Integer -> Integer
scalingFactor n = 2^n

equalityPrecision = 1%(2^256)

instance Num SBDSR where
    fromInteger i
        | i >= 0 = SBDSR (toInteger (length bs)) (SBDS.fromBits bs)
        | otherwise = SBDSR (toInteger (length bs')) (SBDS.neg (SBDS.fromBits bs'))
        where
            bs = integerToBits i
            bs' = integerToBits (-i)

    SBDSR e0 m0 + SBDSR e1 m1 =
           let em = max e0 e1
            in normalise $ withParallelLookahead $ SBDSR (em+1) (SBDS.average (shiftRight m0 (em - e0)) (shiftRight m1 (em - e1)))

    SBDSR e0 m0 - SBDSR e1 m1 =
           let em = max e0 e1
            in normalise $ withParallelLookahead $ SBDSR (em+1) (SBDS.average (shiftRight m0 (em - e0)) (SBDS.neg (shiftRight m1 (em - e1))))

    SBDSR e0 m0 * SBDSR e1 m1 = normalise $ withParallelLookahead $ SBDSR (e0+e1) (SBDS.multiply m0 m1)

    abs _ = error "SDSR.abs: not implemented"
    signum _ = error "SBDSR.signum: not implemented"


instance IntegerDivision SBDSR where
    (SBDSR e m) /#  i = normalise $ withParallelLookahead $ SBDSR e (m /# i)


instance Fractional SBDSR where
    fromRational r = normalise $ withParallelLookahead $ fromInteger (Ratio.numerator r) /# Ratio.denominator r

    SBDSR e0 m0 / SBDSR e1 m1 =
           let (e1_fix, m1') = fixInput m1
            in normalise $ withParallelLookahead $ convertDRSR_SBDSR ((e0-e1+e1_fix+2), (divideSBDS_DRS m0 m1'))
        where
            fixInput = fixInput' 0
                where
                    fixInput' n x =
                        case (x0, x1) of
                            (Z, _) -> fixInput' (n+1) x'
                            (P,M) -> fixInput' (n+1) (cons P x'')
                            (M,P) -> fixInput' (n+1) (cons M x'')
                            _ -> (n, x)
                        where
                            x0 = first x
                            x' = rest x
                            x1 = first x'
                            x'' = rest x''

divideSBDS_DRS :: SBDS -> SBDS -> DRS
divideSBDS_DRS x y =
  case (y0, y1)
    of (Z, _)    -> error "divideToDRS: 1"
       (P, M) -> error "divideToDRS: 2"
       (M, P) -> error "divideToDRS: 3"
       (P, _)    -> divideToDRS' x y
       (M, _)    -> divideToDRS' (SBDS.neg x) (cons P (SBDS.neg y'))

  where
    y0 = first y
    y' = rest y
    y1 = first y'

    divideToDRS' :: SBDS -> SBDS -> DRS
    divideToDRS' x y =
      case (x0, x1)
        of (P, M) -> emit 0 (cons P x'') y
           (M, P) -> emit 0 (cons M x'') y
           (Z, _) -> emit 0 x' y
           (P, _) ->
              let r = SBDS.subtract x y
                  (a, r') = uncons r
                  (a', r'') = uncons r'
               in case a
                    of M ->
                         case a'
                           of P -> emit 2 (cons M r'') y
                              _ ->    emit 1 (rerepresentStarting Z (SBDS.subtract x (cons Z y))) y
                       Z ->        emit 2 r' y
                       P ->
                         case a'
                           of M -> emit 2 (cons P r'') y
                              _ ->    emit 4 (rerepresentStarting Z (SBDS.subtract r y)) y
           (M, _) ->
              let r = SBDS.add x y
                  (a, r') = uncons r
                  (a', r'') = uncons r'
               in case a
                    of P ->
                         case a'
                           of M -> emit (-2) (cons P r'') y
                              _ ->    emit (-1) (rerepresentStarting Z (SBDS.add x (cons Z y))) y
                       Z ->        emit (-2) r' y
                       M ->
                         case a'
                           of P -> emit (-2) (cons M r'') y
                              _    -> emit (-4) (rerepresentStarting Z (SBDS.add r y)) y

      where
        (x0, x')  = uncons x
        (x1, x'')  = uncons x'

    emit :: Int -> SBDS -> SBDS -> DRS
    emit ( 4) x y = DRS DR.one (divideToDRS' x y)
    emit ( 2) x y = DRS DR.half (divideToDRS' x y)
    emit ( 1) x y = DRS DR.quarter (divideToDRS' x y)
    emit ( 0) x y = DRS DR.zero (divideToDRS' x y)
    emit (-1) x y = DRS (DR.neg DR.quarter) (divideToDRS' x y)
    emit (-2) x y = DRS (DR.neg DR.half) (divideToDRS' x y)
    emit (-4) x y = DRS (DR.neg DR.one) (divideToDRS' x y)
    emit n _ _ = error ("emit: " ++ show n)

--
-- NB: Plume's thesis incorrectly suggests that we should add/subtract 1/4, rather than 1/2 in the POne/MOne cases. 
convertDRS_SBDS :: DRS -> SBDS
convertDRS_SBDS (DRS a (DRS b x))
  | a' >= DR.quarter            = cons P (convertDRS_SBDS (DRS (DR.shiftLeftN (DR.subtract a' DR.half) 2) x))
  | a' <= DR.neg DR.quarter     = cons M (convertDRS_SBDS (DRS (DR.shiftLeftN (DR.add a' DR.half) 2) x))
  | otherwise                   = cons Z (convertDRS_SBDS (DRS (DR.shiftLeftN a' 2) x))
  where
    a' = DR.average a (DR.divideByTwo b)

convertDRSR_SBDSR :: (Integer, DRS) -> SBDSR
convertDRSR_SBDSR (e,m) = withParallelLookahead $ SBDSR e ( convertDRS_SBDS m )

normalise = normalise' normaliseIterations

normaliseIterations = 16

normalise' i (SBDSR e m) =
       let (m', de) = SBDS.normalise m i
        in SBDSR (e-toInteger de) m'

forcePrecision (SBDSR e m) n = SBDSR e (SBDS.forcePrecision m n)

showBounds n v = putStr $ unlines $ map show $ cut $ map (\(l,h) -> (fromRational l, fromRational h)) $ (if n > 0 then take n else id) $ toNestedIntervals v
    where
        cut [] = []
        cut ((x,y):t)
            | show x == show y = [(x,y)]
            | otherwise = (x,y):cut t


