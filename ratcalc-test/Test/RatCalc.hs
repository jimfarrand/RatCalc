
import Data.Ratio
import RatCalc.Estimator
import RatCalc.Number.SignedBinaryDigitStreamRepresentation
import Test.QuickCheck

tolerance = 1%(2^256)

prop_approxEqual x = approxEqual tolerance (rational x) x

prop_approxEqualApprox x = approxEqualApprox tolerance (rational x) (rational x)

prop_approxNotEqual x y = x /= y ==> not (approxEqual tolerance (rational x) y)

prop_approxNotEqualApprox x y = x /= y ==> not (approxEqualApprox tolerance (rational x) (rational y))

prop_add x y = approxEqual tolerance (rational x ~+~ rational y) (x+y)

prop_subtract x y = approxEqual tolerance (rational x ~-~ rational y) (x-y)

prop_multiply x y = approxEqual tolerance (rational x ~*~ rational y) (x*y)

prop_divide x y = y /= 0 ==> approxEqual tolerance (rational x ~/~ rational y) (x/y)

myCheck :: Testable prop => prop -> IO ()
myCheck = quickCheckWith args
    where
        args = stdArgs { maxSuccess = 1000 }

main =
    do myCheck prop_approxEqual
       myCheck prop_approxEqualApprox
       myCheck prop_approxNotEqual
       myCheck prop_approxNotEqualApprox
       myCheck prop_add
       myCheck prop_subtract
       myCheck prop_multiply
       myCheck prop_divide
