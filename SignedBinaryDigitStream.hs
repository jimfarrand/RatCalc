
module SignedBinaryDigitStream where

import SignedBinaryDigit hiding (multiply)
import qualified SignedBinaryDigit as SBD
import Estimator
import Debug

data SBDS = Ms | Zs | Ps | Mt SBDS | Zt SBDS | Pt SBDS

instance Eq SBDS where
instance Ord SBDS where

instance Show SBDS where
    show = showBits 50

showBits n x = concat [ "[", show' n x, "]" ]
        where
            show' 0 _ = ".."
            show' _ Ms = "-..."
            show' _ Zs = "0..."
            show' _ Ps = "+..."
            show' n (Mt s) = '-' : show' (n-1) s
            show' n (Zt s) = '0' : show' (n-1) s
            show' n (Pt s) = '+' : show' (n-1) s

mOne = Ms
zero = Zs
pOne = Ps
pHalf = average zero pOne
mHalf = average mOne zero

fromBits [] = Zs
fromBits (True:t) = Pt (fromBits t)
fromBits (False:t) = Zt (fromBits t)

neg :: SBDS -> SBDS
neg Ms = Ps
neg Zs = Zs
neg Ps = Ms
neg (Mt s) = Pt (neg s)
neg (Zt s) = Zt (neg s)
neg (Pt s) = Mt (neg s)

digitMul :: SBD -> SBDS -> SBDS
digitMul M s = neg s
digitMul Z _ = zero
digitMul P s = s

first :: SBDS -> SBD
first Ms = M
first (Mt _) = M
first Zs = Z
first (Zt _) = Z
first Ps = P
first (Pt _) = P

rest :: SBDS -> SBDS
rest Ms = Ms
rest Zs = Zs
rest Ps = Ps
rest (Mt s) = s
rest (Zt s) = s
rest (Pt s) = s

uncons x = (first x, rest x)

incr :: SBDS -> SBDS
incr Ms = Zs
incr Zs = Ps
incr Ps = Ps
incr (Mt s) = Pt s
incr (Zt s) = Pt (incr s)
incr (Pt _) = Ps

decr :: SBDS -> SBDS
decr Ms = Ms
decr Zs = Ms
decr Ps = Zs
decr (Mt _) = Ms
decr (Zt s) = Mt (decr s)
decr (Pt s) = Mt s

cons M = Mt
cons Z = Zt
cons P = Pt


-- Drop the leading digit
shiftLeft1 :: SBDS -> SBDS
shiftLeft1 x = rerepresentStarting Z x

shiftLeft v n
  | n == 0 = v
  | n > 0 = shiftLeft (shiftLeft1 v) (n-1)
  | otherwise = error "shiftLeftN"

shiftN v n
  | n > 0 = shiftRight v n
  | n < 0 = shiftLeft v (-n)
  | otherwise = v

shiftRight s n
    | n == 0 = s
    | n > 0 = shiftRight (cons Z s) (n-1)
    | otherwise = error "shiftRight: negative shift"

average :: SBDS -> SBDS -> SBDS
average x y = average' x y 0
    where
        average' x y c = trace3 "average'" _average' x y c
        -- These are optimisations for infinite streams - there might be more
        {- TODO: Switched off until we know they are safe 
        _average' Ms Ms 0 = Ms
        _average' Zs Zs 0 = Zs
        _average' Ps Ps 0 = Ps
        _average' Ps Ms 0 = Zs
        _average' Ms Ps 0 = Zs
        _average' Ps Zs (-1) = Zs
        _average' Zs Ps (-1) = Zs
        _average' Zs Ms (1) = Zs
        _average' Ms Zs (1) = Zs -}
        _average' x y c
            | even d' = cons (sign d') (average' x' y' 0)
            | otherwise = average'' x' y' d'
            where
                a0 = first x
                b0 = first y
                x' = rest x
                y' = rest y
                d' = digitValue a0 + digitValue b0 + 2*c

        -- Definition in Plume is incorrect - we recurse on x' adn y', not on x'' and y''
        average'' x' y' d' = cons e (average' x' y' c')
            where
                a1 = first x'
                b1 = first y'
                -- x'' = rest x'
                -- y'' = rest y'
                d = 2*d' + digitValue a1 + digitValue b1
                c' = d' - 2*digitValue e
                e = if 2 < d && d <= 6 then
                        P
                    else if (-2) <= d && d <= 2 then
                        Z
                    else if (-6) <= d && d < (-2) then
                        M
                    else
                        error $ concat [ "average'' ",  show x', " ", show y', " ", show d', " ", show d]

        sign x =
            case compare x 0 of
                LT -> M
                EQ -> Z
                GT -> P

-- Re-represent the stream so that it starts with the given digit
-- The returned stream, if appended to the given digit will represent the
-- same number as the given stream, iff such a representation is possible.
-- Otherwise, the result is undefined.
-- This is David Plume's "p function".
-- NB: The digit passed as an argument is NOT returned as part of the result
rerepresentStarting :: SBD -> SBDS -> SBDS
rerepresentStarting P x =
  case x0
    of P -> x'
       Z -> subtractOne x'
       M -> Ms
  where
    (x0, x') = uncons x

rerepresentStarting Z x =
  case x0
    of P -> negateAddOne x' -- Is addOne really doing what we think?  Why no negation here?
       Z -> x'
       M -> subtractOne x'
  where
    (x0, x') = uncons x

rerepresentStarting M x =
  case x0
    of P -> Ps
       Z -> negateAddOne x' -- Is addOne really doing what we think?  Why no negation here?
       M -> x'
  where
    (x0, x') = uncons x

assertNotPositive s ds =
  case ds
    of Ps -> fail
       Pt _ -> fail
       Zt t -> Zt (assertNotPositive s t)
       _ -> ds
  where
    fail = error ("assertNotPositive: " ++ s)

assertNotNegative s ds =
  case ds
    of Ms -> fail
       Mt _ -> fail
       Zt t -> Zt (assertNotNegative s t)
       _ -> ds
  where
    fail = error ("assertNotNegative: " ++ s)

negateAddOne x =
  case x0
    of P -> Ps
       Z -> Pt (negateAddOne x')
       M -> Pt x'
  where
    (x0, x') = uncons x

subtractOne x =
  case x0
    of M -> Ms
       Z -> Mt (subtractOne x')
       P -> Mt x'
  where
    (x0, x') = uncons x


-- Add two streams
-- Undefined if the answer is not in the closed interval [-1,1]
add x y = rerepresentStarting Z (average x y)

-- Subract two streams
-- Undefined if the answer is not in the closed interval [-1,1]
subtract x y = add x (neg y)

multiply = trace2 "multiply" _multiply
_multiply x y = average p q
    where
        a0 = first x
        b0 = first y
        x' = rest x
        y' = rest y
        a1 = first x'
        b1 = first y'
        x'' = rest x'
        y'' = rest y'
        p = average
                (cons
                    (SBD.multiply a0 b1)
                    (average
                        (digitMul b1 x'')
                        (digitMul a1 y'')))
                (average
                    (digitMul b0 x'')
                    (digitMul a0 y''))
        q = cons
                (SBD.multiply a0 b0)
                (cons
                    (SBD.multiply a1 b0)
                    (cons
                        (SBD.multiply a1 b1)
                        (multiply x'' y'')))


-- TODO: Can this be optimised for list ends?
divideByInteger :: SBDS -> Integer -> SBDS
divideByInteger x n
    | n > 0 = divideInteger' x n 0
    | n < 0 = neg (divideInteger' x (-n) 0)
    | otherwise = error "divideByInteger: division by zero"
    where
        divideInteger' = trace3 "divideInteger" _divideInteger'
        _divideInteger' x n s
            | s' >= n            = cons P (divideInteger' x'  n (s'-n))
            | -n < s' && s' < n  = cons Z (divideInteger' x' n s')
            | s' <= -n           = cons M (divideInteger' x' n (s'+n))
            | otherwise = error "bug in divideByInteger"
            where
                a = first x
                x' = rest x
                s' = 2*s + digitValue a

normalise :: SBDS -> Int -> (SBDS, Int)
normalise = normalise' 0
    where
        --normalise' = trace3 "normalise'" _normalise'
        normalise' removed s limit
            | removed >= limit = (s, removed)
            | otherwise =
                case s of
                    Pt (Mt r) -> normalise' (removed+1) (Pt r) limit
                    Zt r ->      normalise' (removed+1) r limit
                    Mt (Pt r) -> normalise' (removed+1) (Mt r) limit
                    _ -> (s, removed)


instance Estimator SBDS where
    toNestedIntervals = bounds' 1 0
        where
            bounds' digit acc s =
                case s of
                    Ms -> [(acc-digit, acc-digit)]
                    Zs -> [(acc, acc)]
                    Ps -> [(acc+digit, acc+digit)]
                    Mt t -> (acc-digit, acc) : bounds' digit' (acc-digit') t
                    Zt t -> (acc-digit', acc+digit') : bounds' digit' acc t
                    Pt t -> (acc, acc+digit) : bounds' digit' (acc+digit') t
                where
                    digit' = digit / 2

prefixDigits [] s = s
prefixDigits (h:t) s = cons h (prefixDigits t s)

forcePrecision orig n = forcePrecision' orig n
    where
        forcePrecision' _ n
            | n == 0 = orig
        forcePrecision' Ms _ = orig
        forcePrecision' Zs _ = orig
        forcePrecision' Ps _ = orig
        forcePrecision' (Mt s) n = forcePrecision' s (n-1)
        forcePrecision' (Zt s) n = forcePrecision' s (n-1)
        forcePrecision' (Pt s) n = forcePrecision' s (n-1)

