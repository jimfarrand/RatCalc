module Xyxyx.RatCalc.ConstructiveRealSpec where

import Test.Hspec
import Test.QuickCheck
import Xyxyx.RatCalc.ConstructiveReal

spec = do
    describe "Constructive Reals" $ do
        it "rationals approximate to correct value" $ property $
            \r -> approximate (rational r) == fromRational r

