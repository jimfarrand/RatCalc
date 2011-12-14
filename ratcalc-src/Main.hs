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
import RatCalc.Data.GenericTree.Zipper as Zipper
import RatCalc.Symbolic.Expression
import RatCalc.Symbolic.Expression.PrettyPrint
import RatCalc.Text.Table
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
          loop terminal

loop terminal =
    do line <- getLineEdited "> "
       case line of
         Nothing -> return ()
         Just line ->
           do case fromString line of
                Left e -> putStrLn $ "Error: " ++ show e
                Right e -> handleExpression terminal e
              loop terminal

handleExpression terminal e = do
    handleExpression' terminal (adorn Set.empty e)

handleExpression' terminal e = do
    putStr "= "
    case tryRules (Set.singleton NewIncoming) (Set.insert Outgoing) rules e of
        Nothing -> do
            runTermOutput terminal (prettyShowExpression terminal e)
            putChar '\n'
        Just ruleResult -> do
            runTermOutput terminal (prettyShowExpression terminal (ruleResultBefore ruleResult))
            putChar '\n'
            handleExpression' terminal (ageIncoming (ruleResultAfter ruleResult))

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


data Subst =
    Subst
        { substMatch   :: Expression
        , substReplace :: forall a. Map String (ExpressionPlus a) -> a -> Maybe (ExpressionPlus a, String)
        }

data Rule =
    Rule
        { ruleOrder :: Order
        , ruleSubst :: Subst
        }

data Order = TopDown | BottomUp

data RuleResult e =
    RuleResult
        { ruleResultBefore      :: ExpressionPlus e
        , ruleResultAfter       :: ExpressionPlus e
        , ruleResultRemoved     :: ExpressionPlus e
        , ruleResultReplacement :: ExpressionPlus e
        , ruleResultExplanation :: String
        }
    deriving (Show)

rules =
    [ ruleAdd
    , ruleMultiply
    ]

ruleAdd :: Rule
ruleAdd =
    rule BottomUp
        ( application (Function "+" True) [symbol "a", symbol "b"] )
        ( \args undecorated ->
            case (numericArg args "a", numericArg args "b") of
                (Just a, Just b) -> Just $ (number undecorated (a+b), "Integer addition")
                _ -> Nothing
        )

ruleMultiply :: Rule
ruleMultiply =
    rule BottomUp
        ( application (Function "*" True) [symbol "a", symbol "b"] )
        ( \args undecorated ->
            case (numericArg args "a", numericArg args "b") of
                (Just a, Just b) -> Just $ (number undecorated (a*b), "Integer multiplication")
                _ -> Nothing
        )

rule :: Order -> Expression -> (forall a. Map String (ExpressionPlus a) -> a -> Maybe (ExpressionPlus a, String)) -> Rule
rule order match replace =
    Rule
        { ruleOrder = order
        , ruleSubst = Subst
            { substMatch = match
            , substReplace = replace
            }
        }

application fn args = Branch fn args

number undecorated n = Leaf (Number n, undecorated)

symbol = Leaf . Symbol

numericArg args a =
    case Map.findWithDefault (error ("numericArg: Unknown arg:" ++ a)) a args of
        Leaf (Number n, _) -> Just n
        _ -> Nothing

tryRules _ _ [] _ = Nothing
tryRules incomingEffects addOutgoingEffect (r0:rs) expr =
    case tryRule incomingEffects addOutgoingEffect r0 expr of
        Nothing -> tryRules incomingEffects addOutgoingEffect rs expr
        r -> r


tryRule :: e -> (e -> e) -> Rule -> ExpressionPlus e -> Maybe (RuleResult e)
tryRule incomingEffect addOutgoingEffect (Rule { ruleOrder = TopDown, ruleSubst = subst }) = trySubstTopDown incomingEffect addOutgoingEffect subst
tryRule incomingEffect addOutgoingEffect (Rule { ruleOrder = BottomUp, ruleSubst = subst }) = trySubstBottomUp incomingEffect addOutgoingEffect subst

trySubstTopDown :: e -> (e -> e) -> Subst -> ExpressionPlus e -> Maybe (RuleResult e)
trySubstTopDown incomingEffect addOutgoingEffect subst expr =
    case trySubstHead incomingEffect addOutgoingEffect subst expr of
        Nothing -> trySubstRecursively (trySubstTopDown incomingEffect addOutgoingEffect) subst expr
        result -> result

trySubstBottomUp :: e -> (e -> e) -> Subst -> ExpressionPlus e -> Maybe (RuleResult e)
trySubstBottomUp incomingEffect addOutgoingEffect subst expr =
    case trySubstRecursively (trySubstBottomUp incomingEffect addOutgoingEffect) subst expr of
        Nothing -> trySubstHead incomingEffect addOutgoingEffect subst expr
        result -> result

trySubstRecursively :: (Subst -> ExpressionPlus e -> Maybe (RuleResult e)) -> Subst -> ExpressionPlus e -> Maybe (RuleResult e)
trySubstRecursively _ _ (Leaf _ ) = Nothing
trySubstRecursively recurse subst (Branch b args) = trySubstRecursively' [] args
    where
        trySubstRecursively' _ [] = Nothing
        trySubstRecursively' acc (arg0:args) =
            case recurse subst arg0 of
                Nothing -> trySubstRecursively' (arg0:acc) args
                Just ruleResult ->
                    Just ruleResult
                        { ruleResultBefore = Branch b (acc' ++ ruleResultBefore ruleResult : args)
                        , ruleResultAfter = Branch b (acc' ++ ruleResultAfter ruleResult : args)
                        }
            where
                acc' = reverse acc

trySubstHead :: e -> (e -> e) -> Subst -> ExpressionPlus e -> Maybe (RuleResult e)
trySubstHead incomingEffect addOutgoingEffect subst expr =
    case tryUnify (substMatch subst) expr of
        Nothing -> Nothing
        Just matches ->
            case substReplace subst matches incomingEffect of
                Nothing -> Nothing
                Just (expr', expl) ->
                    Just RuleResult
                        { ruleResultBefore      = apply addOutgoingEffect expr
                        , ruleResultAfter       = expr'
                        , ruleResultRemoved     = expr
                        , ruleResultReplacement = expr'
                        , ruleResultExplanation = expl
                        }

apply f (Leaf (l, e)) = Leaf (l, f e)
apply f (Branch (b, e) args) = Branch (b, f e) args

tryUnify :: Expression -> ExpressionPlus a -> Maybe (Map String (ExpressionPlus a))
tryUnify = tryUnify' Map.empty
    where
        tryUnify' :: Map String (ExpressionPlus a) -> Expression -> ExpressionPlus a -> Maybe (Map String (ExpressionPlus a))
        tryUnify' a (Leaf (Symbol x)) e = Just (Map.insert x e a)
        tryUnify' a (Leaf (Number n)) (Leaf (Number m, _))
            | n == m = Just a
        tryUnify' a (Branch (Function { functionName = fn1 }) args1) (Branch (Function { functionName = fn2 }, _) args2)
            | fn1 == fn2 = tryUnifyList' a args1 args2
        tryUnify' _ _ _ = Nothing

        tryUnifyList' :: Map String (ExpressionPlus a) -> [Expression] -> [ExpressionPlus a] -> Maybe (Map String (ExpressionPlus a))
        tryUnifyList' a [] [] = Just a
        tryUnifyList' a (h1:t1) (h2:t2) =
            case tryUnify' a h1 h2 of
                Nothing -> Nothing
                Just a' -> tryUnifyList' a' t1 t2
        tryUnifyList' _ _ _ = Nothing

