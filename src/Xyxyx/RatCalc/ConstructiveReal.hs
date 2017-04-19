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

newtype CReal = CReal (Int -> Integer)

instance Show CReal where
  show x = "~" ++ show (approximate x)

rational :: Rational -> CReal
rational r = CReal $ \n -> floor (bN' n * r)

bounds :: CReal -> Int -> (Rational, Rational)
bounds (CReal f) n = ((p-1) % d, (p+1) % d)
  where
    p = f n
    d = bN n


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
  fromInteger n = rational (n%1)

  (CReal x) + (CReal y) = CReal $ add'
    where
      w | b >= 4 = 1
        | b >= 2 = 2
        | otherwise = error "add: b < 2"

      bw = fromIntegral (bN w)

      add' n = round (((x (n+w)) + (y (n+w))) % bw)

  (CReal x) * (CReal y) = CReal $ mul
    where
      (v, w) | b >= 4 = (3,2)
             | b == 3 = (3,3)
             | b == 2 = (4,3)
             | otherwise = error "(CReal.*): n < 2"

      mul n = (signum xpx) * (signum ypy) * round (toRational (1 + abs (xpx * ypy)) / (bN' (px+py-n)))
        where
          min_p = (n+w) `div` 2

          xpx = x px
          ypy = y py

          px = find_p y
          py = find_p x

          find_p z
            | abs (z 0) > 1 = find_p_left (-1)
            | otherwise = find_p_right 1
            where

              find_p_right m
                | l <= min_p = min_p
                | abs (z m) > 1 = l
                | otherwise = find_p_right (m+1)
                where
                  l = n - m + v

              find_p_left m
                | abs (z m) <= 1 = max l min_p
                | otherwise = find_p_left (m-1)
                where
                  l = n - (m+1) + v

  negate (CReal x) = CReal $ \n -> negate (x n)
