import Data.Vect

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

append_nil : Vect m elem -> Vect (plus m 0) elem
append_nil {m} xs = rewrite plusZeroRightNeutral m in xs

append_xs : Vect (S (m + k)) elem -> Vect (plus m (S k)) elem
append_xs {m} {k} xs = rewrite sym (plusSuccRightSucc m k) in xs

append2 : Vect n elem -> Vect m elem -> Vect (m + n) elem
append2 [] ys = append_nil ys
append2 (x :: xs) ys = append_xs (x :: append2 xs ys)
