{-
-- RatCalc - An infinite precision numeric computation framework
-- Copyright (C) 2011 Jim Farrand
--
-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
-- more details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/
-}

{- Run some benchmarks -}

{-# LANGUAGE DoAndIfThenElse #-}

module Main where

import Data.Ratio
import RatCalc.Estimator
import RatCalc.Limits as Limits
import RatCalc.Trigonometry as Trig
import RatCalc.Exponential as Exponential
import RatCalc.Number.SignedBinaryDigitStreamRepresentation
import System.CPUTime
import System.Environment
import System.IO
import Text.Printf
import TestUtils
import Data.List

showResults info count time =
    do putStrLn' "\n#name, count, time, results per time, time per result"
       putStrLn $ concat $ intersperse ", " $ info ++
         [ show count
         , printf "%f" $ toDouble (time%pico)
         , printf "%f" $ toDouble ((count*pico)%time)
         , printf "%f" $ toDouble (time%(count*pico))
         ]
    where
        toDouble :: Rational -> Double
        toDouble = fromRational

pico = 10^12

putStr' s =
    do hPutStr stderr s
       hFlush stdout

putStrLn' s =
    do hPutStrLn stderr s
       hFlush stderr

benchmark' :: [String] -> Integer -> Integer -> Integer -> Integer -> (a -> String) -> (a -> a) -> a -> IO ()
benchmark' name t0 t1 tm count f s state =
  do t2 <- getCPUTime
     if (t2-t0) >= tm then
       showResults name count (t1-t0)
     else
       do putStr' (f state)
          count `seq` benchmark' name t0 t2 tm (count+1) f s (s state)

benchmark name seconds f s state =
    do t0 <- getCPUTime
       benchmark' name t0 t0 (t0+seconds*pico) 0 f s state

benchmarkDigits name seconds x = benchmark name seconds (show . head) tail (unsafeDigits 10 1 x)

benchmarkCalculations name seconds f as = benchmark name seconds (\((a, b):_) -> show (f a b) ++ "\n") tail as


-- Large twin primes
largePrime1 = 65516468355 * 2^333333 - 1
largePrime2 = largePrime1 - 2

runTime = 60

benchmarkPi info = benchmarkDigits info runTime Trig.pi
benchmarkE info = benchmarkDigits info runTime Exponential.e
benchmarkEToPi info = benchmarkDigits info runTime (Exponential.exponential Trig.pi)
benchmarkEToE info = benchmarkDigits info runTime (Exponential.exponential Exponential.e)

r :: Rational -> SBDSR
r = fromRational

benchmarkDivision info = benchmarkCalculations info runTime (\a b -> r a / r b) (diags (allRats ()) (allNonZeroRats ()))

benchmarkMultiplication info = benchmarkCalculations info runTime (\a b -> r a * r b) (diags (allRats ()) (allRats ()))

benchmarkAddition info = benchmarkCalculations info runTime (\a b -> r a + r b) (diags (allRats ()) (allRats ()))

runBenchmark "Division" = benchmarkDivision
runBenchmark "Multiplication" = benchmarkMultiplication
runBenchmark "Addition" = benchmarkAddition
runBenchmark "Pi" = benchmarkPi
runBenchmark "E" = benchmarkE
runBenchmark "E^Pi" = benchmarkEToPi
runBenchmark "E^E" = benchmarkEToE

main =
    do args <- getArgs
       runBenchmark (head args) (swapFront args)

swapFront (h0:h1:t) = (h1:h0:t)
swapFront l = l
