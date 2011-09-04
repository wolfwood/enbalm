-- http://vandreev.wordpress.com/2007/01/07/arithmetic-coding/

module Arith where

import Ratio
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Map as M

type RangeMap k a = [(k, (Ratio a, Ratio a))]

encode :: (Ord k, Integral a) => [k] -> (Int, RangeMap k a, Rational)
encode msg = (length msg, M.assocs freqMap, best $ foldl pair (0,1) rmap)
    where
        freqMap = freqRanges msg
        rmap = map (\x -> fromJust $ M.lookup x freqMap) msg

        best (a,b)                  = approxRational ((b+a)/2) ((b-a)/2)
        pair (a,b) (x,y)            = ((b-a)*x+a, (b-a)*y+a)

decode :: (Ord a, Integral a) => (Int, RangeMap k a, Ratio a) -> [k]
decode (n, freqs, code) = take n $ decode' code
    where
        findChar x = find (\(c, (a,b)) -> (x >= a) && (x < b)) freqs
        decode' code = let
            (Just (c, (x, y))) = findChar code
            in
            c : decode' ((code-x) / (y-x))

freqRanges :: (Ord k, Integral a) => [k] -> M.Map k (Ratio a, Ratio a)
freqRanges str = snd $ M.mapAccum (\acc x -> (acc + x, (acc, acc + x))) 0 freqs
    where
        freqs = M.map (\p -> p % total) occurences
        occurences = foldl (\m c -> M.insertWith (+) c 1 m) M.empty str
        total = sum (M.elems occurences)

encodeToStream msg = let
    (len, freqs, code) = encode msg
    (num, denom) = (numerator code, denominator code)
    bytes n = unfoldr (\k -> if k == 0 then Nothing else Just (rem k 256, quot k 256)) n
    in
    (bytes num, bytes denom)

