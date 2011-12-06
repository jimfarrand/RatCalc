
{-
 - Trigonemtric functions
 -}

module RatCalc.Trigonometry where

import RatCalc.Number.SignedBinaryDigitStreamRepresentation
import RatCalc.Number.RealNumber
import RatCalc.Cauchy as Cauchy
import RatCalc.Limits
import Data.Ratio

arctan :: (Ord a, Fractional a, ToSBDSR a) => a -> RealNumber
arctan x = limit (Cauchy.ranges (converging (arctanIntervals x)))
    where
        arctanIntervals x = arctan' True x 1 0
          where
            arctan' s p d a =
              a' : arctan' (not s) ((x * x) * p) (d+2) a'
              where
                t = p / fromInteger d
                v = if s
                      then t
                      else (negate t)
                a' = a + v

pi = (24 * arctan (r (1%8))) + (8 * arctan (r (1%57)) + (4 * arctan (r (1%239))))
    where
        r :: Rational -> Rational
        r = id
