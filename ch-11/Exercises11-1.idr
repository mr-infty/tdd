module Exercises11_1

import InfList
import Data.Primitives.Views

-----------------------------
-- Exercise 11.1.1
-----------------------------

total
every_other : Stream a -> Stream a
every_other (_ :: (x :: xs)) = x :: every_other xs

-----------------------------
-- Exercise 11.1.2
-----------------------------

Functor InfList where
  map func (value :: xs) = func value :: map func xs

-----------------------------
-- Exercise 11.1.3
-----------------------------

data Face = Heads | Tails

getFace : Int -> Face
getFace k with (divides k 2)
  getFace ((2 * div) + rem) | (DivBy prf) = if rem == 0
                                               then Heads
                                               else Tails

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips count xs = take count (map getFace xs)

-----------------------------
-- Exercise 11.1.3
-----------------------------

square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = let next_approx = (approx + number/approx)/2 in
                                       approx :: square_root_approx number next_approx

-----------------------------
-- Exercise 11.1.4
-----------------------------

-- TODO: Figure out why the definition below throws a type error and implement 
-- version of square_root_bound that uses Approx type.

{-
-- This fails
data Approx : (f : a -> b) -> (y : b) -> (prec : b) -> Type where
  TooManyIterations : (x : a) -> Approx f y prec
  CloseEnough : (Neg b, Abs b, Ord b) => (x : a) -> (pf : abs (y - f x) < prec = True) -> Approx f y prec
-}

-- This works
data Approx : (f : b -> b) -> (y : b) -> (prec : b) -> Type where
  TooManyIterations : (x : b) -> Approx f y prec
  CloseEnough : (Neg b, Abs b, Ord b) => (x : b) -> (pf : abs (y - f x) < prec = True) -> Approx f y prec

{-square_fn : Double -> Double
square_fn x = x * x

square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) ->
                    (approxs : Stream Double) -> Approx Exercises11_1.square_fn number bound
square_root_bound Z number bound (x :: xs) = TooManyIterations x
square_root_bound (S k) number bound (x :: xs) = ?square_root_bound_rhs_1
-}

total
square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) ->
                    (approxs : Stream Double) -> Double
square_root_bound max number bound (x :: xs) = case max of
                                                    Z => x
                                                    (S k) => if abs (number - x*x) < bound
                                                                then x
                                                                else square_root_bound k number bound xs

total
square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000000001
                                       (square_root_approx number number)
