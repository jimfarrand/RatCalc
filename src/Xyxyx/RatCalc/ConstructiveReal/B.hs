
module Xyxyx.RatCalc.ConstructiveReal.B
  ( b
  , bN, divBN
  )
  where

import Data.Array.IArray
import Data.Bits
import Data.List
import Data.Word
import Numeric.Natural

b :: Natural
b = 4

bN :: Word -> Natural
bN n
  | n <= lookupTableMask = lookupTable ! n
  | otherwise = bN' (lookupTable ! lookupTableMask) (n-lookupTableMask)

bN' :: Natural -> Word -> Natural
bN' a n
  | n <= lookupTableMask = (a * lookupTable ! n)
  | otherwise = bN' (a * lookupTable ! lookupTableMask) (n-lookupTableMask)
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
