data InfList : Type -> Type where
  (::) : (value : elem) -> Inf (InfList elem) -> InfList elem
%name InfList xs, ys, zs

total
countFrom : Integer -> InfList Integer
countFrom n = n :: Delay (countFrom (n+1)) --Putting Delay here isn't actually necessary; Idris inserts it automatically

total
getPrefix : (count : Nat) -> InfList ty -> List ty
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs
