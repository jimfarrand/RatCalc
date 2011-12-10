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

{- Expressions representation, parsing and printing -}

import RatCalc.Symbolic.Expression
import RatCalc.Text.Table
import RatCalc.Data.GenericTree
import System.IO

import System.Console.SimpleLineEditor

licenseNotice =
    concat
        [ "ratcalc  Copyright (C) 2010, 2011 Jim Farrand\n"
        , "This program comes with ABSOLUTELY NO WARRANTY.\n"
        , "This is free software, and you are welcome to redistribute it under certain conditions.\n"
        , "See documentation for details.\n"
        ]

evaluate :: Expression -> [(Expression, String)]
evaluate e =
  case simplify e of
    Nothing -> []
    Just (e', explanation) -> (e', explanation) : evaluate e'

simplify :: Expression -> Maybe (Expression, String)
simplify (Leaf (Number _)) = Nothing

-- Additive identity
simplify (Branch (Function { functionName = "+"}) [Leaf (Number 0), y]) = Just (y, "0+x=x")
simplify (Branch (Function { functionName = "+"}) [x, Leaf (Number 0)]) = Just (x, "x+0=x")

-- Multiplicative identity
simplify (Branch (Function { functionName = "*"}) [Leaf (Number 1), y]) = Just (y, "1*x=x")
simplify (Branch (Function { functionName = "*"}) [x, Leaf (Number 1)]) = Just (x, "x*1=x")

-- Multiplication by zero
simplify (Branch (Function { functionName = "*"}) [Leaf (Number 0), _]) = Just (Leaf (Number 0), "0*x=0")
simplify (Branch (Function { functionName = "*"}) [_, Leaf (Number 0)]) = Just (Leaf (Number 0), "x*0=0")

-- Division identity
simplify (Branch (Function { functionName = "/"}) [x, Leaf (Number 1)]) = Just (x, "x/1=x")


-- Integer addition
simplify (Branch (Function { functionName = "+"}) [Leaf (Number x), Leaf (Number y)]) = Just (Leaf (Number z), concat [show x, "+", show y, "=", show z])
    where
        z = x+y

-- Integer multiplication
simplify (Branch (Function { functionName = "*"}) [Leaf (Number x), Leaf (Number y)]) = Just (Leaf (Number z), concat [show x, "*", show y, "=", show z])
    where
        z = x*y

-- Integer power of integer
simplify (Branch (Function { functionName = "^"}) [Leaf (Number x), Leaf (Number y)]) = Just (Leaf (Number z), concat [ show x, "^", show y, "=", show z])
    where
        z = x^y

-- Integer power of rational
simplify (Branch (Function { functionName = "^"}) [Branch (Function { functionName = "/"}) [a, b], Leaf (Number c)]) =
    Just (Branch (Function { functionName = "/"}) [Branch (Function { functionName = "^"}) [a, Leaf (Number c)], Branch (Function { functionName = "^"}) [b, Leaf (Number c)]]
         , concat ["(", show a, "/", show b, ")^", show c, "=(", show a, "^", show c, ")/(", show b, "^", show c, ")"])

simplify (Branch (Function { functionName = "/"}) [Leaf (Number x), Leaf (Number y)])
    | y == 1 = Just (Leaf (Number x), concat [show x, "/1=", show x])


simplify (Branch e args)
    | ok = Just (Branch e args', explanation)
    where
        (ok, args', explanation) =
            case simplify' args of
                Just (e, explanation) -> (True, e, explanation)
                Nothing -> (False, undefined, undefined)
simplify _ = Nothing

{-


-- (x^y)^z = x^(y*z)
simplify (Branch (Function { functionName = "^"}) [Branch (Function { functionName = "^"}) [x, y], z]) = Just (Branch (Function { functionName = "^"}) [x, Branch (Function { functionName = "*"}) [y, z]])



-- x/(-y) -> (-x)/y
-- (n*x)/(x*y) -> x/y
simplify (Branch (Function { functionName = "/"}) [Leaf (Number x), Leaf (Number y)])
    | y == 1 = Just (Leaf (Number x))
    | y < 0 = Just (BrancBranch (Function { functionName = "/"}) [Leaf (Number (-x)), Leaf (Number (-y))])
    | g > 1 = Just (Branch (Function { functionName = "/"}) [Leaf (Number (x) `div` g), Leaf (Number (y) `div` g)])
    where
        g = gcd x y

-- (w/x)*(y/z) -> ((w*y)/(x*z))
simplify (Branch (Function { functionName = "*"}) [Branch (Function { functionName = "/"}) [w, x], Branch (Function { functionName = "/"}) [y, z]]) = Just (Branch (Function { functionName = "/"}) [Branch (Function { functionName = "*"}) [w, y], Branch (Function { functionName = "*"}) [x, z]])

-- x/(y/z) -> x*(z/y)
simplify (Branch (Function { functionName = "/"}) [x, Branch (Function { functionName = "/"}) [y, z]]) = Just (Branch (Function { functionName = "*"}) [x, Branch (Function { functionName = "/"}) [z, y]])

-- (x/y)/z -> x/(y*z)
simplify (Branch (Function { functionName = "/"}) [Branch (Function { functionName = "/"}) [x, y], z]) = Just (Branch (Function { functionName = "/"}) [x, Branch (Function { functionName = "*"}) [y, z]])

-- (x/y)*z -> (x*z)/y
simplify (Branch (Function { functionName = "*"}) [Branch (Function { functionName = "/"}) [x, y], z]) = Just (Branch (Function { functionName = "/"}) [Branch (Function { functionName = "*"}) [x, z], y])
simplify (Branch (Function { functionName = "*"}) [x, Branch (Function { functionName = "/"}) [y, z]]) = Just (Branch (Function { functionName = "/"}) [Branch (Function { functionName = "*"}) [x, y], z])

-- a/b + c/d -> ((a*d)+(c*b))/(b*d)
simplify (Branch (Function { functionName = "+"}) [Branch (Function { functionName = "/"}) [a, b], Branch (Function { functionName = "/"}) [c, d]]) = Just (Branch (Function { functionName = "/"}) [Branch (Function { functionName = "+"}) [Branch (Function { functionName = "*"}) [a, d], Branch (Function { functionName = "*"}) [c,b]], Branch (Function { functionName = "*"}) [b, d]])

-- a + b/c = ((a*c)+b)/c
simplify (Branch (Function { functionName = "+"}) [a, Branch (Function { functionName = "/"}) [b,c]]) = Just (Branch (Function { functionName = "/"}) [Branch (Function { functionName = "+"}) [Branch (Function { functionName = "*"}) [a, c], b], c])
-- a/b + c = (a+(b*c))/b
simplify (Branch (Function { functionName = "+"}) [Branch (Function { functionName = "/"}) [a,b], c]) = Just (Branch (Function { functionName = "/"}) [Branch (Function { functionName = "+"}) [a, Branch (Function { functionName = "*"}) [c, b]], b])


-}

simplify' args = simplify'' [] args
    where
        simplify'' _ [] = Nothing
        simplify'' before (h:t) =
            case simplify h of
                Nothing -> simplify'' (h:before) t
                Just (h', explanation) -> Just (reverse before ++ h':t, explanation)


loop =
    do -- putStr "> "
       -- hFlush stdout
       line <- getLineEdited "> "
       case line of
         Nothing -> return ()
         Just line ->
           do case fromString line of
                Left e -> putStrLn $ "Error: " ++ show e
                Right e -> putStrLn $ unlines $ makeTable " " $ map (\(e,exp) -> [Text LeftAlign 0 "=", Text LeftAlign 0 (show e), Text LeftAlign 0 exp]) $ (e, "") : evaluate e
              loop
                 

main =
    do putStrLn licenseNotice
       initialise
       loop
       restore


