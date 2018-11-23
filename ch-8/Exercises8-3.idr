data MyVect : Nat -> Type -> Type where
  Nil : MyVect 0 elem
  (::) : elem -> MyVect len elem -> MyVect (S len) elem

headUnequal : DecEq a => {xs : MyVect n a} -> {ys : MyVect n a} ->
                         (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a => {xs : MyVect n a} -> {ys : MyVect n a} ->
                         (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl

DecEq a => DecEq (MyVect n a) where
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys) = case decEq x y of
                                   Yes prf_x => (case decEq xs ys of
                                                    Yes prf_xs => Yes (rewrite prf_x in rewrite prf_xs in Refl)
                                                    No contra => No (tailUnequal contra))
                                   No contra => No (headUnequal contra)
