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

module RatCalc.Text.Table where

import Data.List


makeTable sep = makeTable' []
    where
        makeTable' _ [] = []
        makeTable' lens (h:t) = concat h' : makeTable' lens' t
            where
                (h', lens') = alignColumns sep lens h

data Text = Text Align Int String

data Align = LeftAlign | RightAlign

str (Text _ _ s) = s

alignColumns :: String -> [Int] -> [Text] -> ([String], [Int])
alignColumns sep lens d = (formatLine (zip lens' d), lens')
    where
        lens' = map (uncurry max) (zip (lens ++ repeat 0) (map (length . str) d))

        formatLine l = intersperse sep (map (\(len, s) -> pad (len) s) l)

        pad n (Text RightAlign p s) = take (max p (n - length s)) (repeat ' ') ++ s
        pad n (Text LeftAlign p s) = s ++ take (max p (n - length s)) (repeat ' ')

