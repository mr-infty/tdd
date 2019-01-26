module ArithCmd

import Data.Primitives.Views
import System

%default total

public export
data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String

public export
data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

public export
(>>=) : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
(>>=) = Do

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine

public export
data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> ConsoleIO a -> IO (Maybe a)
run _ (Quit x) = pure (Just x)
run Dry _ = do putStrLn "Out of fuel"
               pure Nothing
run (More fuel) (Do action cont) = do res <- runCommand action
                                      run fuel (cont res)

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed) where
  bound : Int -> Int
  bound x with (divides x 12)
    bound ((12 * div) + rem) | (DivBy prf) = rem + 1

{-
quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat
quiz (num1 :: num2 :: nums) score = do PutStr ("Score so far: " ++ show score ++ "\n")
                                       PutStr (show num1 ++ " * " ++ show num2 ++ "? ")
                                       answer <- GetLine
                                       if (cast answer == num1 * num2)
                                          then do PutStr "Correct!"
                                                  quiz nums (score + 1)
                                          else do PutStr ("Wrong, the answer is " ++ show (num1 * num2) ++ "\n")
                                                  quiz nums score
-}

mutual
  correct : Stream Int -> (score : Nat) -> ConsoleIO Nat
  correct nums score = do PutStr "Correct!\n"
                          quiz nums (score + 1)

  wrong : Stream Int -> Int -> (score : Nat) -> ConsoleIO Nat
  wrong nums answer score = do PutStr ("Wrong, the answer is " ++ show answer ++ "\n")
                               quiz nums score

  quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat
  quiz (num1 :: num2 :: nums) score = do PutStr ("Score so far: " ++ show score ++ "\n")
                                         PutStr (show num1 ++ " * " ++ show num2 ++ "? ")
                                         answer <- GetLine
                                         if toLower answer == "quit" then Quit score else
                                           if (cast answer == num1 * num2)
                                              then correct nums score
                                              else wrong nums (num1 * num2) score


partial
main : IO ()
main = do seed <- time
          Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0)
               | Nothing => putStrLn "Ran out of fuel"
          putStrLn ("Final score: " ++ show score)
