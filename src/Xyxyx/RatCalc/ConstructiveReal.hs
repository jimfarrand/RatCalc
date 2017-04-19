{-
-- RatCalc - An arbitrary precision numeric computation framework
-- Copyright © 2017 Jim Farrand
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

  negate _ = error "not implemented"
