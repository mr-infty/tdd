import Expr

Functor Expr where
  map func (Val x) = Val (func x)
  map func (Add x y) = Add (map func x) (map func y)
  map func (Sub x y) = Sub (map func x) (map func y)
  map func (Mul x y) = Mul (map func x) (map func y)
  map func (Div x y) = Div (map func x) (map func y)
  map func (Abs x) = Abs (map func x)

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

Eq ty => Eq (Vect n ty) where
  (==) [] [] = True
  (==) (x :: xs) (x' :: xs') = x == x' && xs == xs'
  (==) _ _ = False

Foldable (Vect n) where
  foldr func acc [] = acc
  foldr func acc (x :: xs) = func x (foldr func acc xs)
