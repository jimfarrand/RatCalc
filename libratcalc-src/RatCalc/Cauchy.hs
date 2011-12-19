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


module RatCalc.Cauchy where

-- This kinda overlaps with Estimator
-- We can unify some of the ideas
-- (Eg, Estimator should probably return [Interval])

import RatCalc.Interval

-- A cauchy sequence, consisting of values which are alterte between above and
-- below the limit
data Cauchy a =
    CauchyBelow (Below a)
  | CauchyAbove (Above a) 

-- |A sequence converging on a limit.  The first value is above the limit
data Above a = Above a (Below a)

-- |A sequence converging on a limit.  The first value is below the limit
data Below a = Below a (Above a)

interval (CauchyBelow (Below l (Above u _))) = makeInterval l u
interval (CauchyAbove (Above u (Below l _))) = makeInterval l u

refine (CauchyAbove (Above _ r)) = (CauchyBelow r)
refine (CauchyBelow (Below _ r)) = (CauchyAbove r)

ranges c = interval c : ranges (refine c)

converging :: Ord a => [a] -> Cauchy a
converging (l@(v0:v1:_))
    | v0 < v1 = CauchyBelow (fromBelow l)
    | v0 > v1 = CauchyAbove (fromAbove l)
    | otherwise = error "converging"

    where
        fromBelow (h:t) = Below h (fromAbove t)
        fromAbove (h:t) = Above h (fromBelow t)

