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

{-# OPTIONS_GHC -XExistentialQuantification  #-}

module RatCalc.Test.QuickCheck.Utils where

import Test.QuickCheck

data QC = forall a. Testable a => QC a

check :: Args -> Bool -> QC -> IO ()
check config verbose (QC a) = (if verbose then verboseCheckWith config else quickCheckWith config) a

checkAll :: Args -> Bool -> [QC] -> IO ()
checkAll _ _ [] = return ()
checkAll config verbose (h:t) = check config verbose h >> checkAll config verbose t

runQuickChecks count verbose checks =
       checkAll (stdArgs { maxSuccess = count }) False checks

