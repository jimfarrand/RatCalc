
module RatCalc.Representation.SignedBinaryDigit where

data SBD = M | Z | P

multiply M M = P
multiply M Z = Z
multiply M P = M
multiply Z M = Z
multiply Z Z = Z
multiply Z P = Z
multiply P M = M
multiply P Z = Z
multiply P P = P

digitValue M = -1
digitValue Z = 0
digitValue P = 1
