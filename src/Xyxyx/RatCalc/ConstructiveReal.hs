
{-

http://ftp.lip6.fr/lip6/reports/2003/lip6.2003.003.pdf

-}

module Xyxyx.RatCalc.ConstructiveReal where

import Xyxyx.RatCalc.ConstructiveReal.B
import Data.Ratio
import Data.Natural

newtype CReal = CReal (Word -> Integer)

instance Show CReal where
  show x = "~" ++ show (approximate x)

rational :: Rational -> CReal
rational r = CReal $ \n -> floor (fromIntegral (bN n) * r)

bounds :: CReal -> Word -> (Rational, Rational)
bounds (CReal f) n = ((p-1) % d, (p+1) % d)
  where
    p = f n
    d = toInteger (bN n)


approximate :: CReal -> Double
approximate r = approximate' 0
  where
    approximate' n
      | lb' == ub' = lb'
      | otherwise = approximate' (n+1)
      where
        (lb, ub) = bounds r n
        lb' = fromRational lb
        ub' = fromRational ub

instance Num CReal where
  (CReal x) + (CReal y) = CReal $ add'
    where
      w | b >= 4 = 1
        | b >= 2 = 2
        | otherwise = error "add: b < 2"

      bw = fromIntegral (bN w)

      add' n = round (((x (n+w)) + (y (n+w))) % bw)

