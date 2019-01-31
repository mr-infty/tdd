module StackIO

import Data.Vect

%default total

data StackCmd : Type -> Nat -> Nat -> Type where
  Push : Integer -> StackCmd () height (S height)
  Pop : StackCmd Integer (S height) height
  Top : StackCmd Integer (S height) (S height)

  GetStr : StackCmd String height height
  PutStr : String -> StackCmd () height height

  Pure : ty -> StackCmd ty height height
  (>>=) : StackCmd a height1 height2 -> (a -> StackCmd b height2 height3) -> StackCmd b height1 height3

runStack : (stk : Vect inHeight Integer) ->
           StackCmd ty inHeight outHeight ->
           IO (ty, Vect outHeight Integer)
runStack stk (Push x) = pure ((), x :: stk)
runStack (x :: xs) Pop = pure (x, xs)
runStack stk@(x :: xs) Top = pure (x, stk)
runStack stk GetStr = do x <- getLine
                         pure (x, stk)
runStack stk (PutStr x) = do putStr x
                             pure ((), stk)
runStack stk (Pure x) = pure (x, stk)
runStack stk (x >>= f) = do (res, newStk) <- runStack stk x
                            runStack newStk (f res)

data StackIO : Nat -> Type where
  Do : StackCmd a height1 height2 ->
       (a -> Inf (StackIO height2)) -> StackIO height1

namespace StackDo
  (>>=) : StackCmd a height1 height2 ->
       (a -> Inf (StackIO height2)) -> StackIO height1
  (>>=) = Do

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> Vect height Integer -> StackIO height -> IO ()
run Dry stk cmd = putStrLn "Out of fuel"
run (More fuel) stk (Do cmd cont) = do (res, newStk) <- runStack stk cmd
                                       run fuel newStk (cont res)

namespace StkInput
  data StkInput = Number Integer
                | Add
                | Sub
                | Mul
                | Neg
                | Pop
                | Dup

strToInput : String -> Maybe StkInput
strToInput "add" = Just Add
strToInput "sub" = Just Sub
strToInput "mul" = Just Mul
strToInput "neg" = Just Neg
strToInput "pop" = Just Pop
strToInput "dup" = Just Dup
strToInput x = if all isDigit (unpack x)
                  then Just (Number (cast x))
                  else Nothing

-- Implentation of the arithmetic operations in terms of the primitive stack operations
doUnaryOp : (Integer -> Integer) -> StackCmd () (S height) (S height)
doUnaryOp f = do val <- Pop
                 Push (f val)

doBinaryOp : (Integer -> Integer -> Integer) -> StackCmd () (S (S height)) (S height)
doBinaryOp f = do val1 <- Pop
                  val2 <- Pop
                  Push (f val1 val2)

doAdd : StackCmd () (S (S height)) (S height)
doAdd = doBinaryOp (\x1, x2 => x1 + x2)

doSub : StackCmd () (S (S height)) (S height)
doSub = doBinaryOp (\x1, x2 => x1 - x2)

doMul : StackCmd () (S (S height)) (S height)
doMul = doBinaryOp (\x1, x2 => x1 * x2)

doNeg : StackCmd () (S height) (S height)
doNeg = doUnaryOp (\x => -x)

doPop : StackCmd Integer (S height) height
doPop = Pop

doDup : StackCmd () (S height) (S (S height))
doDup = do val <- Top
           Push val

mutual
  showMsg : String -> StackIO height
  showMsg x = do PutStr x
                 stackCalc

  stack_underflow : StackIO height
  stack_underflow = showMsg "Stack underflow\n"

  tryAdd : StackIO height
  tryAdd { height = S (S k) } = do doAdd
                                   result <- Top
                                   showMsg (show result ++ "\n")
  tryAdd = stack_underflow

  trySub : StackIO height
  trySub { height = S (S k) } = do doSub
                                   result <- Top
                                   showMsg (show result ++ "\n")
  trySub = stack_underflow

  tryMul : StackIO height
  tryMul { height = S (S k) } = do doMul
                                   result <- Top
                                   showMsg (show result ++ "\n")
  tryMul = stack_underflow

  tryNeg : StackIO height
  tryNeg { height = S k } = do doNeg
                               result <- Top
                               showMsg (show result ++ "\n")
  tryNeg = stack_underflow

  tryPop : StackIO height
  tryPop { height = S k } = do result <- doPop
                               showMsg (show result ++ "\n")
  tryPop = stack_underflow

  tryDup : StackIO height
  tryDup { height = S k } = do doDup
                               result <- Top
                               showMsg ("Duplicated " ++ show result ++ "\n")
  tryDup = stack_underflow

  invalidInput : StackIO height
  invalidInput = showMsg "Invalid input. Please try again.\n"

  processInput : StkInput -> StackIO height
  processInput (Number x) = do Push x
                               stackCalc
  processInput Add = tryAdd
  processInput Sub = trySub
  processInput Mul = tryMul
  processInput Neg = tryNeg
  processInput Pop = tryPop
  processInput Dup = tryDup

  stackCalc : StackIO height
  stackCalc = do PutStr "> "
                 input <- GetStr
                 case strToInput input of
                       Nothing => invalidInput
                       Just x => processInput x

partial
main : IO ()
main = run forever [] stackCalc
