
module Limits where

import Prelude hiding (exponent)

import SignedBinaryDigitStream
import SignedBinaryDigit
import RealArithmetic as R
import Interval
import Cauchy
import Data.Ratio

-- TODO: Problem we have here is that we put the real arithmetic calculations inside a Monad
-- Which seemed like a good idea at the time, but probably isn't
-- If we refactor that, we can probably make this code work fairly easily

limit :: [Interval SBDSR] -> SBDSR
limit (r @ ((Interval { lowerBound = lb, upperBound = ub }):_)) = limit' (max lbe ube) r
  where
    nlb = R.normalise lb
    nub = R.normalise ub
    lbe = exponent nlb
    ube = exponent nub

-- limit' :: Cauchy a b => Integer -> Stream (Interval a) -> SignedBinaryDigitStreamRepresentation
limit' :: Integer -> [Interval SBDSR] -> SBDSR
limit' e (i0:is) =
  SBDSR
    e
    (bananaBracketSBDSR e lb (limit' e is) ub)
  where
    lb = lowerBound i0
    ub = upperBound i0

-- bananaBracketSBDSR :: Integer -> SBDSR -> SBDSR -> SBDSR -> SBDS
bananaBracketSBDSR e x y z  = bananaBracketSBDS (shiftN mx (e-ex)) (shiftN my (e-ey)) (shiftN mz (e-ez))
  where
      ex = exponent x
      ey = exponent y
      ez = exponent z
      mx = mantissa x
      my = mantissa y
      mz = mantissa z

-- bananaBracketSBDS :: SBDS -> SBDS -> SBDS -> SBDS
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

arctan :: SBDSR -> SBDSR
arctan x = limit (Cauchy.ranges (converging (arctanIntervals x)))

-- SOMEHOW, this should be generatlise to work on rationals as well (for performance reasons)
-- Maybe that means we turn ~*~ into a class, or add a Real data type that could be SBDSR or rational
arctanIntervals :: SBDSR -> [SBDSR]
arctanIntervals x = arctan' True x 1 (integer 0)
  where
    arctan' s p d a =
      a' : arctan' (not s) ((x ~*~ x) ~*~ p) (d+2) a'
      where
        t = p ~/# d
        v = if s
              then t
              else (integer 0 ~-~ t)
        a' = a ~+~ v

pi = (24 #*~ arctan (rational (1%8))) ~+~ (8 #*~ arctan (rational (1%57)) ~+~ (4 #*~ arctan (rational (1%239))))

{-

-- fromCauchy :: (Cauchy a b, DivideByInteger b, Ord b) => a -> SBDSR
fromCauchy a = MER e (fromCauchy' lb ub a)
  where
    (e, lb, ub) = findExponent (interval a) 0 1

    findExponent  (i @ (Interval { lowerBound = ilb, upperBound = iub })) e v
      | -v <= ilb && iub <= v = (e, -v, v)
      | otherwise = findExponent i (e+1) (v*2)

-- fromCauchy' :: (Cauchy a b, DivideByInteger b, Ord b) => b -> b -> a -> SBDS
fromCauchy' dlb dub c
  | m <= clb && cub <= dub = cons P (fromCauchy' m dub c)
  | dlb <= clb && cub <= m = cons M (fromCauchy' dlb m c)
  | rlb <= clb && cub <= rub = cons Z (fromCauchy' rlb rub c)
  | otherwise = fromCauchy' dlb dub (refine c)
  where
    m = (dub+dlb) /# 2
    r = (dub-dlb) /# 2
    rlb = m - r
    rub = m + r
    (Interval { lowerBound = clb, upperBound = cub }) = interval c


class HasIntegerDivide a where
    (/#) :: a -> Integer -> a

-- arctan :: SignedBinaryDigitStreamRepresentation -> SBDSR
--arctan x = limit (Cauchy.ranges (converging (arctanIntervals x)))
arctan :: AA (MER SBDS) -> AA (MER SBDS)
arctan x = limit (Cauchy.ranges (converging (arctanIntervals x)))

-- arctanIntervals :: (Fractional a) => a -> [a]
arctanIntervals :: AA (MER SBDS) -> [AA (MER SBDS)]
arctanIntervals x = arctan' True x 1 (approximateInteger 0)
  where
    arctan' s p d a =
      a' : arctan' (not s) ((x ~*~ x) ~*~ p) (d+2) a'
      where
        t = p ~/# d
        v = if s
              then t
              else (approximateInteger 0 ~-~ t)
        a' = a ~+~ v


-- pi = (24 #*~ arctan (approximateRational (1%8))) ~+~ (8 #*~ arctan (approximateRational (1%57)) ~+~ (4 #*~ arctan (approximateRational (1%239))))


-}
