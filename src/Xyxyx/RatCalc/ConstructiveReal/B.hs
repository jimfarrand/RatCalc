{-
-- RatCalc - An arbitrary precision numeric computation framework
-- Copyright © 2017 Jim Farrand
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

module Xyxyx.RatCalc.ConstructiveReal.B
  ( b
  , bN, bN', divBN
  )
  where

import Data.Array.IArray
import Data.Bits
import Data.List
import Data.Word
import Numeric.Natural
import Data.Ratio

b :: Natural
b = 4

bN :: (Integral a, Num b) => a -> b
bN n
   | n >= 0 = fromIntegral $ bN_WN (fromIntegral n)
   | otherwise = error "bN: n < 0"

bN' :: (Integral a, Fractional b) => a -> b
bN' n
  | n > 0 = bN n
  | n < 0 = recip $ bN (-n)
  | otherwise = 1

bN_WN :: Word -> Natural
bN_WN n
  | n <= lookupTableMask = lookupTable ! n
  | otherwise = bN_large (lookupTable ! lookupTableMask) (n-lookupTableMask)
  where
    bN_large :: Natural -> Word -> Natural
    bN_large a n
      | n <= lookupTableMask = (a * lookupTable ! n)
      | otherwise = bN_large (a * lookupTable ! lookupTableMask) (n-lookupTableMask)
      where
        m = min n lookupTableMask
        v = lookupTable ! m


divBN :: (Integral a, Bits a) => a -> Word -> a
divBN
  | b == 2 = div2N
  | b == 4 = div4N
  | otherwise = divBN'
  where
    div2N x n = x `shiftR` (fromIntegral n)
    div4N x n = x `shiftR` (fromIntegral (n `shiftL` 2))
    divBN' x n = x `div` (fromIntegral (bN n))
  

lookupTableSize :: Word
lookupTableSize = 7

lookupTableMask :: Word
lookupTableMask = (2^lookupTableSize) - 1

lookupTableMask' :: Natural
lookupTableMask' = fromIntegral lookupTableMask


lookupTable :: Array Word Natural
lookupTable = listArray (0, lookupTableMask) (iterate (*b) 1)
