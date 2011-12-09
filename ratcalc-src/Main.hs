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
simplify (Number _) = Nothing

-- Additive identity
simplify (Application (Symbol "+") [Number 0, y]) = Just (y, "0+x=x")
simplify (Application (Symbol "+") [x, Number 0]) = Just (x, "x+0=x")

-- Multiplicative identity
simplify (Application (Symbol "*") [Number 1, y]) = Just (y, "1*x=x")
simplify (Application (Symbol "*") [x, Number 1]) = Just (x, "x*1=x")

-- Multiplication by zero
simplify (Application (Symbol "*") [Number 0, y]) = Just (Number 0, "0*x=0")
simplify (Application (Symbol "*") [x, Number 0]) = Just (Number 0, "x*0=0")

-- Division identity
simplify (Application (Symbol "/") [x, Number 1]) = Just (x, "x/1=x")


-- Integer addition
simplify (Application (Symbol "+") [Number x, Number y]) = Just (Number z, concat [show x, "+", show y, "=", show z])
    where
        z = x+y

-- Integer multiplication
simplify (Application (Symbol "*") [Number x, Number y]) = Just (Number z, concat [show x, "*", show y, "=", show z])
    where
        z = x*y

-- Integer power of integer
simplify (Application (Symbol "^") [Number x, Number y]) = Just (Number z, concat [ show x, "^", show y, "=", show z])
    where
        z = x^y

-- Integer power of rational
simplify (Application (Symbol "^") [Application (Symbol "/") [a, b], Number c]) =
    Just (Application (Symbol "/") [Application (Symbol "^") [a, Number c], Application (Symbol "^") [b, Number c]]
         , concat ["(", show a, "/", show b, ")^", show c, "=(", show a, "^", show c, ")/(", show b, "^", show c, ")"])

simplify (Application (Symbol "/") [Number x, Number y])
    | y == 1 = Just (Number x, concat [show x, "/1=", show x])


simplify (Application e args)
    | ok = Just (Application e args', explanation)
    where
        (ok, args', explanation) =
            case simplify' args of
                Just (e, explanation) -> (True, e, explanation)
                Nothing -> (False, undefined, undefined)
simplify _ = Nothing

{-


-- (x^y)^z = x^(y*z)
simplify (Application (Symbol "^") [Application (Symbol "^") [x, y], z]) = Just (Application (Symbol "^") [x, Application (Symbol "*") [y, z]])



-- x/1 -> x
-- x/(-y) -> (-x)/y
-- (n*x)/(x*y) -> x/y
simplify (Application (Symbol "/") [Number x, Number y])
    | y == 1 = Just (Number x)
    | y < 0 = Just (Application (Symbol "/") [Number (-x), Number (-y)])
    | g > 1 = Just (Application (Symbol "/") [Number (x `div` g), Number (y `div` g)])
    where
        g = gcd x y

-- (w/x)*(y/z) -> ((w*y)/(x*z))
simplify (Application (Symbol "*") [Application (Symbol "/") [w, x], Application (Symbol "/") [y, z]]) = Just (Application (Symbol "/") [Application (Symbol "*") [w, y], Application (Symbol "*") [x, z]])

-- x/(y/z) -> x*(z/y)
simplify (Application (Symbol "/") [x, Application (Symbol "/") [y, z]]) = Just (Application (Symbol "*") [x, Application (Symbol "/") [z, y]])

-- (x/y)/z -> x/(y*z)
simplify (Application (Symbol "/") [Application (Symbol "/") [x, y], z]) = Just (Application (Symbol "/") [x, Application (Symbol "*") [y, z]])

-- (x/y)*z -> (x*z)/y
simplify (Application (Symbol "*") [Application (Symbol "/") [x, y], z]) = Just (Application (Symbol "/") [Application (Symbol "*") [x, z], y])
simplify (Application (Symbol "*") [x, Application (Symbol "/") [y, z]]) = Just (Application (Symbol "/") [Application (Symbol "*") [x, y], z])

-- a/b + c/d -> ((a*d)+(c*b))/(b*d)
simplify (Application (Symbol "+") [Application (Symbol "/") [a, b], Application (Symbol "/") [c, d]]) = Just (Application (Symbol "/") [Application (Symbol "+") [Application (Symbol "*") [a, d], Application (Symbol "*") [c,b]], Application (Symbol "*") [b, d]])

-- a + b/c = ((a*c)+b)/c
simplify (Application (Symbol "+") [a, Application (Symbol "/") [b,c]]) = Just (Application (Symbol "/") [Application (Symbol "+") [Application (Symbol "*") [a, c], b], c])
-- a/b + c = (a+(b*c))/b
simplify (Application (Symbol "+") [Application (Symbol "/") [a,b], c]) = Just (Application (Symbol "/") [Application (Symbol "+") [a, Application (Symbol "*") [c, b]], b])


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


