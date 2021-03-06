module Stack

import Data.Vect

data StackCmd : Type -> Nat -> Nat -> Type where
  Push : Integer -> StackCmd () height (S height)
  Pop : StackCmd Integer (S height) height
  Top : StackCmd Integer (S height) (S height)

  Pure : ty -> StackCmd ty height height
  (>>=) : StackCmd a height1 height2 -> (a -> StackCmd b height2 height3) -> StackCmd b height1 height3

testAdd : StackCmd Integer 0 0
testAdd = do Push 10
             Push 20
             val1 <- Pop
             val2 <- Pop
             Pure (val1 + val2)

runStack : (stk : Vect inHeight Integer) ->
           StackCmd ty inHeight outHeight ->
           (ty, Vect outHeight Integer)
runStack stk (Push x) = ((), x :: stk)
runStack (x :: xs) Pop = (x, xs)
runStack stk@(x :: xs) Top = (x, stk)
runStack stk (Pure x) = (x, stk)
runStack stk (x >>= f) = let (res, newStk) = runStack stk x in
                             runStack newStk (f res)
