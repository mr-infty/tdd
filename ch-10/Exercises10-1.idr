module Exercises10_1

-----------------------------
-- Exercise 10.1
-----------------------------

data Take : (n : Nat) -> (xs : List a) -> Type where
  Fewer : {auto isFewer : LT (length xs) n} -> Take n xs
  Exact : (n_xs : List a) -> {auto isEqual : (length n_xs = n)} -> Take n (n_xs ++ rest)

take : (n : Nat) -> (input : List a) -> Take n input
take Z input = Exact []
take (S k) [] = Fewer
take (S k) (x :: xs) = case Exercises10_1.take k xs of --Need to be explicit because `take` is overloaded
                            Fewer {isFewer} => Fewer {isFewer = LTESucc isFewer}
                            Exact n_xs {isEqual} => Exact (x :: n_xs) {isEqual = cong isEqual}

groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN n xs with (Exercises10_1.take n xs)
  groupByN n xs | Fewer = [xs]
  groupByN n (n_xs ++ rest) | (Exact n_xs) = n_xs :: groupByN n rest

-----------------------------
-- Division with Remainder
-----------------------------

total
subtractFromLarger : (a : Nat) ->
                     (b : Nat) ->
                     {auto isLarger : LTE b a}
                     -> (c : Nat ** (b + c = a))
subtractFromLarger a Z {isLarger = LTEZero} = (a ** Refl)
subtractFromLarger (S right) (S left) {isLarger = (LTESucc x)} = case subtractFromLarger right left {isLarger = x} of
                                                                      (c ** pf) => (c ** cong {f = S} pf)

total
ifNotSmallerThenGTE : (contra : LT a b -> Void) -> LTE b a
ifNotSmallerThenGTE contra {a = Z} {b = Z} = LTEZero
ifNotSmallerThenGTE contra {a = Z} {b = (S k)} with (contra (LTESucc (LTEZero {right = k})))
  ifNotSmallerThenGTE contra {a = Z} {b = (S k)} | with_pat impossible
ifNotSmallerThenGTE contra {a = (S k)} {b = Z} = LTEZero
ifNotSmallerThenGTE contra {a = (S k)} {b = (S j)} = LTESucc (ifNotSmallerThenGTE (\prf => contra (LTESucc prf)))

total
eqIsCommutative : (a = b) -> (b = a)
eqIsCommutative Refl = Refl

inductionStep : (eqPf : x = plus (mult q (S k)) r) ->
                (pf : S (plus k x) = a)
                -> a = S (plus (plus k (mult q (S k))) r)
inductionStep eqPf pf = ?inductionStep_rhs

divWithRem : (a : Nat) ->
             (b : Nat) ->
             {auto bNotZ : Not (b = Z)}
             -> (q : Nat ** r : Nat ** ((a = q*b + r), LT r b))
divWithRem a Z {bNotZ} with (bNotZ Refl)
  divWithRem a Z {bNotZ} | with_pat impossible
divWithRem a (S k) {bNotZ} = case isLTE (S a) (S k) of
                                  Yes prf => (0 ** a ** (Refl, prf))
                                  No contra => (case subtractFromLarger a (S k) {isLarger = (ifNotSmallerThenGTE contra)} of
                                                     (x ** pf) => (case divWithRem x (S k) {bNotZ = uninhabited} of
                                                                        (q' ** r ** (eqPf, remPf)) => (S q' ** r ** (inductionStep eqPf pf, remPf))))


-----------------------------
-- Exercise 10.2
-----------------------------

halves : List a -> (List a, List a)
halves xs = let n = div (length xs) 2 in
                (case Exercises10_1.take n xs of
                      Fewer {isFewer} => ?halves_rhs_1
                      Exact n_xs {rest} => ?halves_rhs_2)
