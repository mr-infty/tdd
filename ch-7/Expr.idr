module Expr

public export
data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

public export
eval : (Neg num, Integral num, Abs num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

public export
Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

public export
Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub

public export
Abs ty => Abs (Expr ty) where
  abs = Abs
