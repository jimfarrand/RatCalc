
module Main where

import System.CPUTime
import RealArithmetic
import Limits
import Text.Printf
import Data.Ratio
import System.IO
import Estimator

benchmark v t0 current step =
    do putStr (show current)
       putStr "bits:\t"
       t1 <- getCPUTime
       let v' = forcePrecision v current
        in do putStr (show v')
              t2 <- getCPUTime
              putStr ": "
              putStr (printf "%10.3f" $ (fromInteger (t2-t1) / 10^12 :: Double))
              putStr " "
              putStr (printf "%10.3f" $ (fromInteger (t2-t0) / 10^12 :: Double))
              putStrLn "s"
              benchmark v' t0 (current+step) step

benchmark2 ds t0 n m =
    do putStr (show (head ds))
       if n `mod` m == 0 then
           do t1 <- getCPUTime
              putStr ": "
              let t = (t1-t0)%(10^12)
                  td = t / fromInteger n
              putStrLn (printf "%10d %10.3fs %10.3fs/digit" n (fromRational t :: Double) (fromRational td :: Double))
       else return ()
       hFlush stdout
       benchmark2 (tail ds) t0 (n+1) m

main =
    do t0 <- getCPUTime
       --benchmark Limits.pi t0 64 64
       benchmark2 (unsafeDigits 10 1 Limits.pi) t0 1 50
