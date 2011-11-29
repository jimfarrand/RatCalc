
module Debug where

import Data.Set as Set
import Debug.Trace

debug = False

on =
    Set.fromList
        [ "normalise'"
        ]

isOn name = Set.member name on

trace2 name f =
    if debug && isOn name then
        trace2On name f
    else
        traceOff f

trace3 name f =
    if debug && isOn name then
        trace3On name f
    else
        traceOff f

traceOff f = f

trace2On name f a0 a1
    | debug = trace (concat [name, " (", show a0, ") (", show a1, ")"]) (f a0 a1)
    | otherwise = f a0 a1

trace3On name f a0 a1 a2
    | debug = trace (concat [name, " (", show a0, ") (", show a1, ") (", show a2, ")"]) (f a0 a1 a2)
    | otherwise = f a0 a1 a2

