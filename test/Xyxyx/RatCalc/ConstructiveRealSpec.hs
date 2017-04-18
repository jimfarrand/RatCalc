module Xyxyx.RatCalc.ConstructiveRealSpec where

import Test.Hspec
import Test.QuickCheck
import Xyxyx.RatCalc.ConstructiveReal

spec = do
    describe "Constructive Reals" $ do
        it "rationals approximate to correct value" $ property $
            \r -> approximate (rational r) == fromRational r
        it "added rationals approximate to correct value" $ property $
            \x y -> approximate ((rational x) `add` (rational y)) == fromRational (x+y)

