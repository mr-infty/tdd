import Data.Vect

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = rewrite plusZeroRightNeutral m in Refl
myPlusCommutes (S k) m = rewrite sym (plusSuccRightSucc m k) in cong (myPlusCommutes k m)

reverseProof_nil : Vect n a -> Vect (plus n 0) a
reverseProof_nil {n} xs = rewrite plusZeroRightNeutral n in xs

reverseProof_xs : Vect ((S n) + m) a -> Vect (plus n (S m)) a
reverseProof_xs {n} {m} xs = rewrite sym (plusSuccRightSucc n m) in xs

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs where
  reverse' : Vect n a -> Vect m a -> Vect (n+m) a
  reverse' acc [] = reverseProof_nil acc
  reverse' acc (x :: ys) = reverseProof_xs (reverse' (x :: acc) ys)
