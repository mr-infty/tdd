import EqNat

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case checkEqNat m len of
                                 Nothing => Nothing
                                 Just (Same len) => Just input

exactLengthWithDecEq : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLengthWithDecEq {m} len input = case decEq m len of
                                          Yes prf => Just rewrite sym prf in input
                                          No contra => Nothing
