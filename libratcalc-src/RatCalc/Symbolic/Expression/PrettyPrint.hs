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


module RatCalc.Symbolic.Expression.PrettyPrint where

import Data.List as List
import Data.Monoid
import RatCalc.Data.GenericTree hiding (map)
import RatCalc.Estimator
import RatCalc.Symbolic.Expression hiding (showExpression)
import qualified RatCalc.Data.GenericTree as GenericTree

type ExpressionPlus a = GenericTree (Term, a) (Function, a)

adorn :: e -> Expression -> ExpressionPlus e
adorn e = GenericTree.map (\x -> (x, e)) (\x -> (x, e))

unadorn = GenericTree.map fst fst

showExpression (Leaf (Integer x, effect))
    | x >= 0 = effect (show x)
    | otherwise = effect ("(" ++ show x ++ ")") -- FIXME: This is necessary, because -12^4 = -(12^4), not (-12)^4
showExpression (Leaf (Real x, effect))
    | x >= 0 = effect (showEstimatorDigits 10 10 x)
    | otherwise = effect ("(" ++ showEstimatorDigits 10 10 x ++ ")") -- FIXME: This is necessary, because -12^4 = -(12^4), not (-12)^4
showExpression (Leaf (Symbol x, effect)) = effect x
showExpression (Branch (Function { functionName = name, infixOperator = True }, effect) args) =
    effect "(" `mappend` (mconcat $ List.intersperse (effect name) $ map showExpression args) `mappend` effect ")"
showExpression (Branch (Function { functionName = name, infixOperator = False }, effect) args) =
    mconcat [ effect name,  effect "(", mconcat (List.intersperse (effect ",") (map showExpression args)), effect ")" ]
