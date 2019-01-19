module Exercises10_2

import Data.List.Views
import Data.Vect
import Data.Vect.Views
import Data.Nat.Views

-----------------------------
-- Exercise 10.2.1
-----------------------------

total
equalSuffix : Eq a => List a -> List a -> List a
equalSuffix xs ys with (snocList xs)
  equalSuffix [] ys | Empty = []
  equalSuffix (xs ++ [x]) ys | (Snoc xsrec) with (snocList ys)
    equalSuffix (xs ++ [x]) [] | (Snoc xsrec) | Empty = []
    equalSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xsrec) | (Snoc ysrec) = case x == y of
                                                                             False => equalSuffix xs ys | xsrec | ysrec
                                                                             True => (equalSuffix xs ys | xsrec | ysrec) ++ [x]

-----------------------------
-- Exercise 10.2.2
-----------------------------

total
mergeSort : Ord a => Vect n a -> Vect n a
mergeSort input with (splitRec input)
  mergeSort [] | SplitRecNil = []
  mergeSort [x] | SplitRecOne = [x]
  mergeSort (lefts ++ rights) | (SplitRecPair lrec rrec) = merge (mergeSort lefts | lrec) (mergeSort rights | rrec)

-----------------------------
-- Exercise 10.2.3
-----------------------------

total
toBinary : Nat -> String
toBinary k with (halfRec k)
  toBinary Z | HalfRecZ = "0"
  toBinary (n + n) | (HalfRecEven rec) = toBinary n | rec ++ "0"
  toBinary (S (n + n)) | (HalfRecOdd rec) = let s = toBinary n | rec in
                                                if s == "0" then "1" else s ++ "1"

-----------------------------
-- Exercise 10.2.4
-----------------------------

total
palindrome : Eq a => List a -> Bool
palindrome input with (vList input)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (xs ++ [y])) | (VCons rec) = case x == y of
                                                     False => False
                                                     True => palindrome xs | rec
