
module ShowTable where

import Data.List

alignColumns :: [Int] -> [String] -> ([String], [Int])
alignColumns lens d = (formatLine (zip lens' d), lens')
    where
        lens' = map (uncurry max) (zip (lens ++ repeat 0) (map length d))

        formatLine l = intersperse " | " (map (\(len, s) -> pad (len) s) l)

        pad n s = take (max 0 (n - length s)) (repeat ' ') ++ s

