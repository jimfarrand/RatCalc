
module Cauchy where

-- This kinda overlaps with Estimator
-- We can unify some of the ideas
-- (Eg, Estimator should probably return [Interval])

import Interval

data Cauchy a =
    CauchyBelow (Below a)
  | CauchyAbove (Above a) 

-- |A sequence converging on a limit.  This value is above the limit
data Above a = Above a (Below a)

-- |A sequence converging on a limit.  This value is below the limit
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
