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

{-# LANGUAGE RankNTypes #-}

import Data.Map (Map)
import Data.Set (Set)
import RatCalc.Data.GenericTree hiding (map)
import RatCalc.Symbolic.Expression
import RatCalc.Symbolic.Expression.PrettyPrint
import System.Console.SimpleLineEditor
import System.Console.Terminfo.Base
import System.Console.Terminfo.Color
import System.IO
import System.Console.Terminfo.Effects
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified RatCalc.Data.GenericTree as GenericTree
import qualified RatCalc.Symbolic.Expression.PrettyPrint as PP

licenseNotice =
    concat
        [ "ratcalc  Copyright (C) 2010, 2011 Jim Farrand\n"
        , "This program comes with ABSOLUTELY NO WARRANTY.\n"
        , "This is free software, and you are welcome to redistribute it under certain conditions.\n"
        , "See documentation for details.\n"
        ]

-------------------
-- Repl
-------------------

type Effects = Set Effect

data Effect = Outgoing | NewIncoming | OldIncoming
    deriving (Eq, Ord, Show)


main = do initialise
          terminal <- setupTermFromEnv
          putStr licenseNotice
          loop terminal


loop terminal =
    do line <- getLineEdited "> "
       case line of
         Nothing -> return ()
         Just line -> do
            case fromString line of
                Left e -> putStrLn $ "Error: " ++ show e
                Right e -> handleExpression terminal (adorn Set.empty e)
            putChar '\n'
            loop terminal

handleExpression terminal e =
    case tryRules rules (unadorn e) of
        Nothing -> do
            putStr "= "
            runTermOutput terminal (prettyShowExpression terminal e)
            putChar '\n'
        Just (path, replacement, expl) ->
            let replacement' = adorn (Set.singleton NewIncoming) replacement
                e' = subst path replacement' e in do
                -- putStrLn (show path)
                putStr "= "
                runTermOutput terminal (prettyShowExpression terminal (markOutgoing path e))
                putChar '\n'
                handleExpression terminal (ageIncoming e')

markOutgoing path e = subst path (f (get path e)) e
    where
        -- f = GenericTree.map g g
        -- g (l, e) = (l, Set.insert Outgoing e)
        f (Leaf (l, e)) = Leaf (l, g e)
        f (Branch (b, e) args)= Branch (b, g e) args
        g = Set.insert Outgoing

get [] e = e
get (h:t) (Branch _ args) = get t (args !! h)
get _ _ = error "get"

ageIncoming =
    GenericTree.map
        ( \(l, e) -> (l, Set.fromList (concatMap f (Set.elems e))) )
        ( \(b, e) -> (b, Set.fromList (concatMap f (Set.elems e))) )
    where
        f NewIncoming = [OldIncoming]
        f OldIncoming = []
        f Outgoing = [Outgoing]




prettyShowExpression :: Terminal -> ExpressionPlus (Set Effect) -> TermOutput
prettyShowExpression terminal expr = PP.showExpression $ reifyEffects terminal (propogateEffects Set.empty expr)

propogateEffects :: Ord a => Set a -> ExpressionPlus (Set a) -> ExpressionPlus (Set a)
propogateEffects es' (Leaf (l, es)) = Leaf (l, Set.union es es')
propogateEffects es' (Branch (l, es) args) = Branch (l, es'') (map (propogateEffects es'') args)
    where
        es'' = Set.union es es'

reifyEffects terminal = GenericTree.map (\(l, e) -> (l, f e)) (\(b, e) -> (b, f e))
    where
        f = applyEffects terminal

applyEffects terminal effects s
    | effects == Set.empty = termText s
    | effects == Set.singleton OldIncoming = applyAttributes terminal (defaultAttributes { boldAttr = True }) (applyColor terminal Yellow (termText s))
    | effects == Set.singleton Outgoing = applyAttributes terminal (defaultAttributes { underlineAttr = True }) (applyColor terminal Cyan (termText s))
    | effects == Set.fromList [OldIncoming, Outgoing] = applyAttributes terminal (defaultAttributes { boldAttr = True, underlineAttr = True } ) (applyColor terminal Green (termText s))
    | otherwise = error "applyEffects: unknown effect list"

applyAttributes terminal attributes s =
    case getCapability terminal withAttributes of
        Just f -> f attributes s
        Nothing -> s

applyColor terminal color s =
    case getCapability terminal withForegroundColor of
        Just f -> f color s
        Nothing -> s




-------------------
-- Rules
-------------------

rules =
    [ ruleAdditionIdentity
    , ruleMultiplicationIdentity
    , ruleDivisionIdentity
    , ruleIntegerAddition
    , ruleIntegerMultiplication
    , ruleIntegerDivision
    , ruleRationalAddition
    , ruleRationalAddition'
    , ruleRationalAddition''
    , ruleRationalMultiplication
    , ruleRationalDivision
    ]

ruleAdditionIdentity :: Rule
ruleAdditionIdentity =
    Rule
        { substMatch = application (Function "+" True) [symbol "a", symbol "b"] 
        , substReplace =
              \args ->
                case (numericArg args "a", arg args "b") of
                    (Just 0, Just b) -> Just $ (b, "Addition identity")
                    _ -> case (arg args "a", numericArg args "b") of
                            (Just a, Just 0) -> Just (a, "Addition identity")
                            _ -> Nothing
        }

ruleMultiplicationIdentity :: Rule
ruleMultiplicationIdentity =
    Rule
        { substMatch = application (Function "*" True) [symbol "a", symbol "b"] 
        , substReplace =
              \args ->
                case (numericArg args "a", arg args "b") of
                    (Just 1, Just b) -> Just $ (b, "Multiplication identity")
                    _ -> case (arg args "a", numericArg args "b") of
                            (Just a, Just 1) -> Just (a, "Multiplication identity")
                            _ -> Nothing
        }


ruleDivisionIdentity :: Rule
ruleDivisionIdentity =
    Rule
        { substMatch = application (Function "/" True) [symbol "a", symbol "b"] 
        , substReplace =
              \args ->
                    case (arg args "a", numericArg args "b") of
                        (Just a, Just 1) -> Just (a, "Division identity")
                        _ -> Nothing
        }

ruleIntegerAddition :: Rule
ruleIntegerAddition =
    Rule
        { substMatch = application (Function "+" True) [symbol "a", symbol "b"] 
        , substReplace =
              \args ->
                case (numericArg args "a", numericArg args "b") of
                    (Just a, Just b) -> Just $ (number (a+b), "Integer addition")
                    _ -> Nothing
        }

ruleIntegerMultiplication :: Rule
ruleIntegerMultiplication =
    Rule
        { substMatch = application (Function "*" True) [symbol "a", symbol "b"] 
        , substReplace =
              \args ->
                case (numericArg args "a", numericArg args "b") of
                    (Just a, Just b) -> Just $ (number (a*b), "Integer multiplication")
                    _ -> Nothing
        }

ruleIntegerDivision :: Rule
ruleIntegerDivision =
    Rule
        { substMatch = application (Function "/" True) [symbol "a", symbol "b"] 
        , substReplace =
              \args ->
                case (numericArg args "a", numericArg args "b") of
                    (Just a, Just b) ->
                        let g = gcd a b in
                        if g == 1 then
                            Nothing
                        else
                            Just $ (application (Function "/" True) [number (a `div` g), number (b `div` g)], "Integer multiplication")
                    _ -> Nothing
        }

ruleRationalAddition :: Rule
ruleRationalAddition =
    Rule
        { substMatch = application (Function "+" True) [application (Function "/" True) [symbol "a", symbol "b"], application (Function "/" True) [symbol "c", symbol "d"]]
        , substReplace =
            \args ->
                case (arg args "a", arg args "b", arg args "c", arg args "d") of
                    (Just a, Just b, Just c, Just d) ->
                        Just $ (application (Function "/" True) [application (Function "+" True) [application (Function "*" True) [a, d], application (Function "*" True) [c, b]], application (Function "*" True) [b, d]], "Rational addition")
                    _ -> Nothing
        }

ruleRationalAddition' :: Rule
ruleRationalAddition' =
    Rule
        { substMatch = application (Function "+" True) [symbol "a", application (Function "/" True) [symbol "c", symbol "d"]]
        , substReplace =
            \args ->
                case (arg args "a", arg args "c", arg args "d") of
                    (Just a, Just c, Just d) ->
                        Just $ (application (Function "/" True) [application (Function "+" True) [application (Function "*" True) [a, d], c], d], "Rational addition")
                    _ -> Nothing
        }

ruleRationalAddition'' :: Rule
ruleRationalAddition'' =
    Rule
        { substMatch = application (Function "+" True) [application (Function "/" True) [symbol "a", symbol "b"], symbol "c"]
        , substReplace =
            \args ->
                case (arg args "a", arg args "b", arg args "c") of
                    (Just a, Just b, Just c) ->
                        Just $ (application (Function "/" True) [application (Function "+" True) [a, application (Function "*" True) [c, b]], b], "Rational addition")
                    _ -> Nothing
        }



ruleRationalMultiplication :: Rule
ruleRationalMultiplication =
    Rule
        { substMatch = application (Function "*" True) [application (Function "/" True) [symbol "a", symbol "b"], application (Function "/" True) [symbol "c", symbol "d"]]
        , substReplace =
            \args ->
                case (arg args "a", arg args "b", arg args "c", arg args "d") of
                    (Just a, Just b, Just c, Just d) ->
                        Just $ (application (Function "/" True) [application (Function "*" True) [a, c], application (Function "*" True) [b, d]], "Rational multiplication")
                    _ -> Nothing

        }

ruleRationalDivision :: Rule
ruleRationalDivision =
    Rule
        { substMatch = application (Function "/" True) [application (Function "/" True) [symbol "a", symbol "b"], application (Function "/" True) [symbol "c", symbol "d"]]
        , substReplace =
            \args ->
                case (arg args "a", arg args "b", arg args "c", arg args "d") of
                    (Just a, Just b, Just c, Just d) ->
                        Just $ (application (Function "*" True) [application (Function "/" True) [a, b], application (Function "/" True) [d, c]], "Rational division")
                    _ -> Nothing

        }



application fn args = Branch fn args

number n = Leaf (Number n)

symbol = Leaf . Symbol

arg :: Map String Expression -> String -> Maybe Expression
arg args a = Map.lookup a args

numericArg :: Map String Expression -> String -> Maybe Integer
numericArg args a =
    case Map.lookup a args of
        Just (Leaf (Number n)) -> Just n
        _ -> Nothing



-------------------
-- Rule handling
-------------------

type Path = [Int]

-- Perform a substitution
subst :: Path -> GenericTree b l -> GenericTree b l -> GenericTree b l
subst [] e _ = e
subst (h:t) e (Branch b args) = Branch b (subst' h t e [] args)
    where
        subst' n path e before (h:t)
            | n == 0 = reverse before ++ (subst path e h):t
            | otherwise = subst' (n-1) path e (h:before) t
        subst' _ _ _ _ [] = error "subst'"
subst _ _ _ = error "subst"

data Rule =
    Rule
        { substMatch   :: Expression
        , substReplace :: Map String Expression -> Maybe (Expression, String)
        }

tryRules :: [Rule] -> Expression -> Maybe (Path, Expression, String)
tryRules rules expr =
    case tryRulesHead rules expr of
        Just (expr, expl) -> Just ([], expr, expl)
        Nothing -> case expr of
            Leaf _ -> Nothing
            Branch _ args -> tryRules' 0 [] args
    where
        tryRules' _ _ [] = Nothing
        tryRules' n before (h:t) =
            case tryRules rules h of
                Just (path, expr,expl) -> Just (n:path, expr, expl) -- Just (n:path, reverse before ++ expr:t, expl)
                Nothing -> tryRules' (n+1) (h:before) t

tryRulesHead [] _ = Nothing
tryRulesHead (h:t) expr =
    case tryRuleHead h expr of
        Nothing -> tryRulesHead t expr
        r -> r

tryRuleHead :: Rule -> Expression -> Maybe (Expression, String)
tryRuleHead rule expr =
    case tryUnify (substMatch rule) expr of
        Nothing -> Nothing
        Just matches -> substReplace rule matches

tryUnify :: Expression -> Expression -> Maybe (Map String Expression)
tryUnify = tryUnify' Map.empty
    where
        tryUnify' :: Map String Expression -> Expression -> Expression -> Maybe (Map String Expression)
        tryUnify' a (Leaf (Symbol x)) e = Just (Map.insert x e a)
        tryUnify' a (Leaf (Number n)) (Leaf (Number m))
            | n == m = Just a
        tryUnify' a (Branch (Function { functionName = fn1 }) args1) (Branch (Function { functionName = fn2 }) args2)
            | fn1 == fn2 = tryUnifyList' a args1 args2
        tryUnify' _ _ _ = Nothing

        tryUnifyList' :: Map String Expression -> [Expression] -> [Expression] -> Maybe (Map String Expression)
        tryUnifyList' a [] [] = Just a
        tryUnifyList' a (h1:t1) (h2:t2) =
            case tryUnify' a h1 h2 of
                Nothing -> Nothing
                Just a' -> tryUnifyList' a' t1 t2
        tryUnifyList' _ _ _ = Nothing

