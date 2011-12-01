
module RatCalc.Representation.DyadicRational where


-- Normalised so that the numerator is always odd
data DR =
  DR
    { numerator   :: Integer
    , denominator :: Integer
    }
  deriving (Eq)


instance Show DR where
  show dr = show (numerator dr) ++ "/2^" ++ show (denominator dr)

instance Ord DR where
  compare x y = compare nx ny
    where
      (nx, ny, _) =
         makeDenominatorsEqual
           (numerator x)
           (denominator x)
           (numerator y)
           (denominator y)

dyadicRational :: Integer -> Integer -> DR
dyadicRational a b
  | a == 0 = DR { numerator = 0, denominator = 0 }
  | b < 0 = error "dyadicRational: b < 0"
  | b > 0 && even a = dyadicRational (a `div` 2) (b - 1)
  -- abs a > 2^b = error "dyadicRati
  | otherwise = DR { numerator = a
                               , denominator = b
                               }

neg dr = dr { numerator = negate (numerator dr) }

zero = dyadicRational 0 0
one = dyadicRational 1 0
half = dyadicRational 1 1
quarter = dyadicRational 1 2

shiftLeftN x n
  | n < 0 = error "shiftLeftN: n < 0"
  | n == 0 = x
  | num == 0 = zero
  | den' < 0 = error $ "shiftLeftN " ++ show x ++ " " ++ show n ++ ": result > 1"
  | otherwise = dyadicRational num den'
  where
    num = numerator x
    den = denominator x
    den' = den - n

-- To add two dyadic rationals, first make the numerators the same
add x y = dyadicRational (nx+ny) d
  where
    (nx, ny, d) =
       makeDenominatorsEqual
         (numerator x)
         (denominator x)
         (numerator y)
         (denominator y)

subtract x y = add x (neg y)

average :: DR -> DR -> DR
average v0 v1
  | b > d = DR { numerator = a + (c * (2 ^ (b - d))), denominator = b + 1 }
  | b < d = DR { numerator = (a * (2 ^ (d - b))) + c, denominator = d + 1 }
  | otherwise = dyadicRational (a+c) (b+1)
  where
    a = numerator v0
    b = denominator v0
    c = numerator v1
    d = denominator v1

divideByTwo dr = shiftRightN dr 1

-- TODO: No need to use dyadicRational here, as numerator will always be odd.
shiftRightN dr n
  | n < 0 = error "shiftRightN"
  | n == 0 = dr
  | otherwise = dyadicRational (numerator dr) (n + denominator dr)



makeDenominatorsEqual nx dx ny dy
  | dx < dy = makeDenominatorsEqual (nx*2) (dx+1) ny dy
  | dx > dy = makeDenominatorsEqual nx dx (ny*2) (dy+1)
  | otherwise = (nx, ny, dx)
