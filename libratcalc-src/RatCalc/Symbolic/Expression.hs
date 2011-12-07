
module RatCalc.Symbolic.Expression where

data Expression =
      Constant Integer
    | Variable String
    | Application Expression [Expression]


