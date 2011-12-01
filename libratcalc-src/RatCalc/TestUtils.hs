
module TestUtils where

import Data.Ratio

positiveRats () = map (mkRat . curry ungcd 1) (boolseqs ())
    where
        mkRat (m,n) = m%n
        boolseqs () = [] : [b : bs | bs <- boolseqs (), b <- [False, True]]


allRats () = 0 : allNonZeroRats ()

allNonZeroRats () = interleave [positiveRats (), map negate (positiveRats ())]

allIntegers () = interleave [[0..], (map negate [1..])]
nonZeroIntegers () = interleave [[1..], (map negate [1..])]

ungcd (d, bs) = foldr undo (d,d) bs
    where undo False (m, n) = (m, n+m)
          undo True (m, n) = (m+n, n)

interleave l = map head l ++ interleave (map tail l)

diags l r = concat (diags' [] [[ (m,n) | m <- l] | n <- r])
    where
        diags' xss (ys:yss) = map head xss : diags' (ys:map tail xss) yss
        diags' _ [] = error "diags: non infinite list"
