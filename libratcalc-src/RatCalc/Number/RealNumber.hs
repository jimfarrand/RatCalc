
module RatCalc.Number.RealNumber where

import RatCalc.Number.SignedBinaryDigitStreamRepresentation
import RatCalc.Estimator

data RealNumber = RRational Rational | RSBDSR SBDSR
    deriving (Show, Eq, Ord)

instance Num RealNumber where
    fromInteger = RRational . fromInteger

    RRational a + RRational b = RRational (a+b)
    RSBDSR a + RRational b = RSBDSR (a + fromRational b)
    RRational a + RSBDSR b = RSBDSR (fromRational a + b)
    RSBDSR a + RSBDSR b = RSBDSR (a+b)

    RRational a - RRational b = RRational (a-b)
    RSBDSR a - RRational b = RSBDSR (a - fromRational b)
    RRational a - RSBDSR b = RSBDSR (fromRational a - b)
    RSBDSR a - RSBDSR b = RSBDSR (a-b)

    RRational a * (RRational b) = RRational (a*b)
    RSBDSR a * (RRational b) = RSBDSR (a * fromRational b)
    RRational a * (RSBDSR b) = RSBDSR (fromRational a * b)
    RSBDSR a * (RSBDSR b) = RSBDSR (a*b)

instance Fractional RealNumber where
    fromRational = RRational

instance ToSBDSR RealNumber where
    toSBDSR (RSBDSR n) = n
    toSBDSR (RRational n) = fromRational n

instance Estimator RealNumber where
    toNestedIntervals (RRational a) = [(a,a)]
    toNestedIntervals (RSBDSR a) = toNestedIntervals a
