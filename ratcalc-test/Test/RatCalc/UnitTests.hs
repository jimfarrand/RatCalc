
{-# OPTIONS_GHC -XDoAndIfThenElse #-}

module Test.RatCalc.UnitTests where

import Test.HUnit
import System.Exit

tests = TestList []

runTests = runTestTT tests

main = do count <- runTests
          if errors count > 0 then
            exitWith (ExitFailure 1)
          else if failures count > 0 then
            exitWith (ExitFailure 2)
          else
            return ()
