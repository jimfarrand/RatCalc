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

{-# LANGUAGE DoAndIfThenElse #-}

import Data.Map (Map)
import Data.Set (Set)
import RatCalc.Data.GenericTree hiding (map)
import RatCalc.Number.ExactReal
import RatCalc.Symbolic.Expression as Expression
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
    "This is ratcalc.  Copyright (C) 2010, 2011 Jim Farrand.  This program comes\n"
    ++ "with ABSOLUTELY NO WARRANTY.  This is free software, and you are welcome to\n"
    ++ "redistribute it under certain conditions.  See documentation for details.\n"

-------------------
-- Repl
-------------------

type Effects = Set Effect

data Effect = Outgoing | NewIncoming | OldIncoming
    deriving (Eq, Ord, Show)


data Context =
    Context
        { contextTerminal :: Terminal
        , contextExpressionWidth :: Int
        }

data State =
    State
        { stateBindings :: Map Symbol ExactReal
        }

-- Initialise the terminal and then enter the main loop.
main = do initialise
          terminal <- setupTermFromEnv
          putStrLn licenseNotice
          loop
            Context
                { contextTerminal = terminal
                , contextExpressionWidth = 0
                }

-- The main loop.  Print a prompt, parse it and then handle it
loop context =
    do line <- getLineEdited "$ "
       case line of
         Nothing -> return ()
         Just "" -> loop context
         Just "\EOT" -> return ()
         Just line -> do
            case fromString line of
                Left e -> do
                    putStrLn $ "Error: " ++ show e
                    loop context
                Right e -> do
                    context <- handleExpression context (adorn Set.empty e)
                    putChar '\n'
                    loop context

-- Handle a parsed expression entered by the user
handleExpression context e =
    case tryRules rules (unadorn e) of
        Nothing -> do
            putStr "= "
            context <- printExpression context False e
            putChar '\n'
            return context
        Just (path, replacement, rule) ->
            let replacement' = adorn (Set.singleton NewIncoming) replacement
                e' = subst path replacement' e in do
                putStr "= "
                context <- printExpression context True (markOutgoing path e)
                putStr (substExpl rule)
                putChar '\n'
                handleExpression context (ageIncoming e')

-- Mark a path with the OutGoing effect
markOutgoing :: Path -> ExpressionPlus Effects -> ExpressionPlus Effects
markOutgoing path e = subst path (f (dereferencePath path e)) e
    where
        -- f = GenericTree.map g g
        -- g (l, e) = (l, Set.insert Outgoing e)
        f (Leaf (l, e)) = Leaf (l, g e)
        f (Branch (b, e) args)= Branch (b, g e) args
        g = Set.insert Outgoing

-- Make NewIncoming nodes OldIncoming, and remove OldIncoming
ageIncoming :: ExpressionPlus Effects -> ExpressionPlus Effects
ageIncoming =
    GenericTree.map
        ( \(l, e) -> (l, Set.fromList (concatMap f (Set.elems e))) )
        ( \(b, e) -> (b, Set.fromList (concatMap f (Set.elems e))) )
    where
        f NewIncoming = [OldIncoming]
        f OldIncoming = []
        f Outgoing = [Outgoing]

-- Find the sub-expression indicated by the given path
dereferencePath :: [Int] -> GenericTree t t1 -> GenericTree t t1
dereferencePath [] e = e
dereferencePath (h:t) (Branch _ args) = dereferencePath t (args !! h)
dereferencePath _ _ = error "dereferncePath"

-- Print an appropriately padded expression.  Update the padding in the context.
printExpression :: Context -> Bool -> ExpressionPlus Effects -> IO Context
printExpression context pad e = do
    runTermOutput terminal (prettyShowExpression terminal e)
    if pad then
        putStr (take (width-len+2) (repeat ' '))
    else
        return ()
    return context { contextExpressionWidth = width }
  where
    len = length (Expression.showExpression (unadorn e))
    width = max (contextExpressionWidth context) len
    terminal = contextTerminal context


-- Turn a marked up expression into terminal output
prettyShowExpression :: Terminal -> ExpressionPlus (Set Effect) -> TermOutput
prettyShowExpression terminal expr = PP.showExpression $ reifyEffects terminal (propogateEffects Set.empty expr)

-- Propogate effects to children
propogateEffects :: Ord a => Set a -> ExpressionPlus (Set a) -> ExpressionPlus (Set a)
propogateEffects es' (Leaf (l, es)) = Leaf (l, Set.union es es')
propogateEffects es' (Branch (l, es) args) = Branch (l, es'') (map (propogateEffects es'') args)
    where
        es'' = Set.union es es'

-- Turn effects into functions that apply the appropriate markup
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
                    (Just 0, Just b) -> Just b
                    _ -> case (arg args "a", numericArg args "b") of
                            (Just a, Just 0) -> Just a
                            _ -> Nothing
        , substExpl = "Addition identity"
        }

ruleMultiplicationIdentity :: Rule
ruleMultiplicationIdentity =
    Rule
        { substMatch = application (Function "*" True) [symbol "a", symbol "b"] 
        , substReplace =
              \args ->
                case (numericArg args "a", arg args "b") of
                    (Just 1, Just b) -> Just b
                    _ -> case (arg args "a", numericArg args "b") of
                            (Just a, Just 1) -> Just a
                            _ -> Nothing
        , substExpl = "Multiplication identity"
        }


ruleDivisionIdentity :: Rule
ruleDivisionIdentity =
    Rule
        { substMatch = application (Function "/" True) [symbol "a", symbol "b"] 
        , substReplace =
              \args ->
                    case (arg args "a", numericArg args "b") of
                        (Just a, Just 1) -> Just a
                        _ -> Nothing
        , substExpl = "Division identity"
        }

ruleIntegerAddition :: Rule
ruleIntegerAddition =
    Rule
        { substMatch = application (Function "+" True) [symbol "a", symbol "b"] 
        , substReplace =
              \args ->
                case (numericArg args "a", numericArg args "b") of
                    (Just a, Just b) -> Just $ number (a+b)
                    _ -> Nothing
        , substExpl = "Integer addition"
        }

ruleIntegerMultiplication :: Rule
ruleIntegerMultiplication =
    Rule
        { substMatch = application (Function "*" True) [symbol "a", symbol "b"] 
        , substReplace =
              \args ->
                case (numericArg args "a", numericArg args "b") of
                    (Just a, Just b) -> Just $ number (a*b)
                    _ -> Nothing
        , substExpl = "Integer multiplication"
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
                            Just $ application (Function "/" True) [number (a `div` g), number (b `div` g)]
                    _ -> Nothing
        , substExpl = "Integer division"
        }

ruleRationalAddition :: Rule
ruleRationalAddition =
    Rule
        { substMatch = application (Function "+" True) [application (Function "/" True) [symbol "a", symbol "b"], application (Function "/" True) [symbol "c", symbol "d"]]
        , substReplace =
            \args ->
                case (arg args "a", arg args "b", arg args "c", arg args "d") of
                    (Just a, Just b, Just c, Just d) ->
                        Just $ application (Function "/" True) [application (Function "+" True) [application (Function "*" True) [a, d], application (Function "*" True) [c, b]], application (Function "*" True) [b, d]]
                    _ -> Nothing
        , substExpl = "Rational addition"
        }

ruleRationalAddition' :: Rule
ruleRationalAddition' =
    Rule
        { substMatch = application (Function "+" True) [symbol "a", application (Function "/" True) [symbol "c", symbol "d"]]
        , substReplace =
            \args ->
                case (arg args "a", arg args "c", arg args "d") of
                    (Just a, Just c, Just d) ->
                        Just $ application (Function "/" True) [application (Function "+" True) [application (Function "*" True) [a, d], c], d]
                    _ -> Nothing
        , substExpl = "Integer/Rational addition"
        }

ruleRationalAddition'' :: Rule
ruleRationalAddition'' =
    Rule
        { substMatch = application (Function "+" True) [application (Function "/" True) [symbol "a", symbol "b"], symbol "c"]
        , substReplace =
            \args ->
                case (arg args "a", arg args "b", arg args "c") of
                    (Just a, Just b, Just c) ->
                        Just $ application (Function "/" True) [application (Function "+" True) [a, application (Function "*" True) [c, b]], b]
                    _ -> Nothing
        , substExpl = "Rational/Integer addition"
        }



ruleRationalMultiplication :: Rule
ruleRationalMultiplication =
    Rule
        { substMatch = application (Function "*" True) [application (Function "/" True) [symbol "a", symbol "b"], application (Function "/" True) [symbol "c", symbol "d"]]
        , substReplace =
            \args ->
                case (arg args "a", arg args "b", arg args "c", arg args "d") of
                    (Just a, Just b, Just c, Just d) ->
                        Just $ application (Function "/" True) [application (Function "*" True) [a, c], application (Function "*" True) [b, d]]
                    _ -> Nothing

        , substExpl = "Rational multiplication"
        }

ruleRationalDivision :: Rule
ruleRationalDivision =
    Rule
        { substMatch = application (Function "/" True) [application (Function "/" True) [symbol "a", symbol "b"], application (Function "/" True) [symbol "c", symbol "d"]]
        , substReplace =
            \args ->
                case (arg args "a", arg args "b", arg args "c", arg args "d") of
                    (Just a, Just b, Just c, Just d) ->
                        Just $ application (Function "*" True) [application (Function "/" True) [a, b], application (Function "/" True) [d, c]]
                    _ -> Nothing

        , substExpl = "Rational division"
        }



application fn args = Branch fn args

number n = Leaf (Number n)

symbol = Leaf . Symbol

arg :: Map Symbol Expression -> Symbol -> Maybe Expression
arg args a = Map.lookup a args

numericArg :: Map Symbol Expression -> Symbol -> Maybe Integer
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
        , substReplace :: Map Symbol Expression -> Maybe Expression
        , substExpl    :: String
        }

instance Show Rule where
    show (Rule { substMatch = match, substExpl = expl }) =
        concat
            [ "Rule { substMatch = "
            , show match
            , ", substExpl = "
            , show expl
            , "}"
            ]

tryRules :: [Rule] -> Expression -> Maybe (Path, Expression, Rule)
tryRules rules expr =
    case tryRulesHead rules expr of
        Just (expr', rule) -> Just ([], expr', rule)
        Nothing -> case expr of
            Leaf _ -> Nothing
            Branch _ args -> tryRules' 0 [] args
    where
        tryRules' _ _ [] = Nothing
        tryRules' n before (h:t) =
            case tryRules rules h of
                Just (path, expr,expl) -> Just (n:path, expr, expl)
                Nothing -> tryRules' (n+1) (h:before) t

tryRulesHead :: [Rule] -> Expression -> Maybe (Expression, Rule)
tryRulesHead [] _ = Nothing
tryRulesHead (h:t) expr =
    case tryRuleHead h expr of
        Nothing -> tryRulesHead t expr
        Just expr' -> Just (expr', h)

tryRuleHead :: Rule -> Expression -> Maybe Expression
tryRuleHead rule expr =
    case tryUnify (substMatch rule) expr of
        Nothing -> Nothing
        Just matches -> substReplace rule matches

-- Unify expression.  Return the substitution from symbols in the left
-- expression, that will make it equal to the right expression
tryUnify :: Expression -> Expression -> Maybe (Map Symbol Expression)
tryUnify = tryUnify' Map.empty
    where
        tryUnify' :: Map Symbol Expression -> Expression -> Expression -> Maybe (Map Symbol Expression)
        tryUnify' a (Leaf (Symbol x)) e = Just (Map.insert x e a)
        tryUnify' a (Leaf (Number n)) (Leaf (Number m))
            | n == m = Just a
        tryUnify' a (Branch (Function { functionName = fn1 }) args1) (Branch (Function { functionName = fn2 }) args2)
            | fn1 == fn2 = tryUnifyList' a args1 args2
        tryUnify' _ _ _ = Nothing

        tryUnifyList' :: Map Symbol Expression -> [Expression] -> [Expression] -> Maybe (Map Symbol Expression)
        tryUnifyList' a [] [] = Just a
        tryUnifyList' a (h1:t1) (h2:t2) =
            case tryUnify' a h1 h2 of
                Nothing -> Nothing
                Just a' -> tryUnifyList' a' t1 t2
        tryUnifyList' _ _ _ = Nothing

