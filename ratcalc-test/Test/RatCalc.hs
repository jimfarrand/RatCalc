
import Data.Ratio
import RatCalc.Estimator
import RatCalc.Number.SignedBinaryDigitStreamRepresentation
import Test.QuickCheck

tolerance = 1%(2^256)

i :: Integer -> SBDSR
i = fromInteger

r :: Rational -> SBDSR
r = fromRational

prop_approxEqual x = approxEqual tolerance (r x) x

prop_approxEqualApprox x = approxEqualApprox tolerance (r x) (r x)

prop_approxNotEqual x y = x /= y ==> not (approxEqual tolerance (r x) y)

prop_approxNotEqualApprox x y = x /= y ==> not (approxEqualApprox tolerance (r x) (r y))

prop_add x y = approxEqual tolerance (r x + r y) (x+y)

prop_subtract x y = approxEqual tolerance (r x - r y) (x-y)

prop_multiply x y = approxEqual tolerance (r x * r y) (x*y)

prop_divide x y = y /= 0 ==> approxEqual tolerance (r x / r y) (x/y)

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
