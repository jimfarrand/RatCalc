
module Main where

import SignedBinaryDigitStream
--import MantissaExponentRepresentation
--import ApproximateArithmetic
import RealArithmetic
import Estimator
import Data.Ratio
import TestUtils
import Control.Concurrent
import System.CPUTime
import ShowTable
import System.IO
import qualified Data.Map as Map
import Text.Printf
import Limits

type TestStats = Map.Map String (Integer, Integer)

updateTestStats :: TestStats -> String -> Integer -> (TestStats, Integer, Rational)
updateTestStats stats name time = (Map.insert name (newCount, newTime) stats, newCount, averageTime)
    where
        (oldCount, oldTime) = Map.findWithDefault (0, 0) name stats
        newCount = oldCount + 1
        newTime = oldTime + time
        averageTime = newTime % newCount

emptyStats = Map.empty


test1 :: (Show a, Show r) => String -> (a -> r) -> (a -> r -> Bool) -> a -> (String, Bool, [String])
test1 name f t a =
       let r = f a
           o = t a r
           r' = r
           o' = o
           output = [name, show a, "", show r', show o']
        in (name, o', output)

test2 :: (Show a0, Show a1, Show r) => [Char] -> (a0 -> a1 -> r) -> (a0 -> a1 -> r -> Bool) -> a0 -> a1 -> (String, Bool, [String])
test2 name f t a b =
       let r = f a b
           o = t a b r
           r' = r
           o' = o
           output = [name, show a, show b, show r', show o']
        in (name, o', output)

runTests nice lens stats (h:t) =
    do t0 <- getCPUTime
       let (name, success, output) = h
        in do t1 <- seq success getCPUTime
              let td = t1 - t0
                  (stats', count, averageTime) = updateTestStats stats name td
                  (formatted, lens') = alignColumns lens (show count : printf "%.6fms" ((fromRational averageTime / 10^9) :: Double) : output)
               in do putStrsLn formatted
                     if success then
                       do let delay = nice * (fromInteger (td `div` 10^6))
                           in if delay > 0 then threadDelay delay
                              else return ()
                          runTests nice lens' stats' t
                      else
                        return ()

putStrsLn [] =
    do putChar '\n'
       hFlush stdout
putStrsLn (h:t) =
    do putStr h
       hFlush stdout
       putStrsLn t


tests () = interleave
    [ testApproxIntegers ()
    , testApproxRationals ()
    , testApproxEquals ()
    , testApproxEqualApproxs ()
    , testAdds ()
    , testSubtracts ()
    , testMultiplies ()
    , testDivideByIntegers ()
    , testDivides ()
    ]

mer = id

testApproxIntegers () = map testApproxInteger (allIntegers ())
testApproxRationals () = map testApproxRational (allRats ())
testApproxEquals () = map (uncurry testApproxEqual) (diags (allRats ()) (allRats ()))
testApproxEqualApproxs () = map (uncurry testApproxEqualApprox) (diags (allRats ()) (allRats ()))
testMultiplies () = map (uncurry testMultiply) (diags (allRats ()) (allRats ()))
testAdds () = map (uncurry testAdd) (diags (allRats ()) (allRats ()))
testSubtracts () = map (uncurry testSubtract) (diags (allRats ()) (allRats ()))
testDivideByIntegers () = map (uncurry testDivideByInteger) (diags (allRats ()) (nonZeroIntegers ()))
testDivides () = map (uncurry testDivide) (diags (allRats ()) (allNonZeroRats ()))

testApproxInteger = test1 "integer" (\a -> integer a) (\a r -> fromInteger a =~ r)
testApproxRational = test1 "rational" (\a -> rational a) (\a r -> a =~ r)

testApproxEqual = test2 "~=" (\a b -> (mer $ rational a) ~= b) (\a b r -> if a == b then r else not r)
testApproxEqualApprox = test2 "~=~" (\a b -> (mer $ rational a) ~=~ (mer $ rational b)) (\a b r -> if a == b then r else not r)
testMultiply = test2 "~*~" (\a b -> (rational a ~*~ rational b)) (\a b r -> a*b =~ r)
testAdd = test2 "~+~" (\a b -> (rational a ~+~ rational b)) (\a b r -> a+b =~ r)
testSubtract = test2 "~-~" (\a b -> (rational a ~-~ rational b)) (\a b r -> a-b =~ r)
testDivideByInteger = test2 "~/#" (\a b -> (mer $ rational a) ~/# b) (\a b r -> (a / fromInteger b) =~ r)
testDivide = test2 "~/~" (\a b -> (mer $ rational a) ~/~ rational b) (\a b r -> (a / b) =~ r)

main = runTests 0 [] emptyStats (tests ())
