
module RatCalc.Interval where

data Interval a =
  Interval
    { lowerBound :: a
    , upperBound :: a
    } deriving (Show, Eq)

makeInterval l u
    | l <= u = Interval { lowerBound = l, upperBound = u }

mapInterval f i = Interval { lowerBound = f (lowerBound i), upperBound = f (upperBound i) }
