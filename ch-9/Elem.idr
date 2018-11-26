data MyVect : Nat -> Type -> Type where
  Nil : MyVect 0 a
  (::) : a -> MyVect n a -> MyVect (S n) a

data Elem : a -> MyVect k a -> Type where
  Here : Elem x (x :: xs)
  There : (later : Elem x xs) -> Elem x (y :: xs)

notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There _) impossible

notInTail : (notHere : (value = x) -> Void) -> (notThere : Elem value xs -> Void) -> Elem value (x :: xs) -> Void
notInTail notHere notThere Here = notHere Refl
notInTail notHere notThere (There later) = notThere later

isElem : DecEq a => (value : a) -> (xs : MyVect n a) -> Dec (Elem value xs)
isElem value [] = No notInNil
isElem value (x :: xs) = case decEq value x of
                              Yes Refl => Yes Here
                              No notHere => (case isElem value xs of
                                            Yes prf => Yes (There prf)
                                            No notThere => No (notInTail notHere notThere))
