import Expr

Show ty => Show (Expr ty) where
  show (Val x) = show x
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " div " ++ show y ++ ")"
  show (Abs x) = "|" ++ show x ++ "|"

(Eq ty, Neg ty, Integral ty, Abs ty) => Eq (Expr ty) where
  (==) x y = eval x == eval y

(Neg ty, Integral ty, Abs ty) => Cast (Expr ty) ty where
  cast x = eval x
