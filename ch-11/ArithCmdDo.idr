module ArithCmdDo

import Data.Primitives.Views
import System

%default total

public export
data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String

  Pure : ty -> Command ty
  Bind : Command a -> (a -> Command b) -> Command b

public export
data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace CommandDo
  public export
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleDo
  public export
  (>>=): Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

public export
runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (Pure x) = pure x
runCommand (Bind c f) = do res <- runCommand c
                           runCommand (f res)

public export
data Input = Answer Int
           | QuitCmd

public export
readInput : (prompt : String) -> Command Input
readInput prompt = do PutStr prompt
                      answer <- GetLine
                      if toLower answer == "quit"
                         then Pure QuitCmd
                         else Pure (Answer (cast answer))

mutual
  correct : Stream Int -> (score : Nat) -> ConsoleIO Nat
  correct nums score = do PutStr "Correct!\n"
                          quiz nums (score + 1)

  wrong : Stream Int -> Int -> (score : Nat) -> ConsoleIO Nat
  wrong nums answer score = do PutStr ("Wrong, the answer is " ++ show answer ++ "\n")
                               quiz nums score
  quiz : Stream Int -> (score : Nat) -> ConsoleIO Nat
  quiz (num1 :: num2 :: nums) score = do PutStr ("Score so far: " ++ show score ++ "\n")
                                         input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
                                         case input of
                                               Answer answer => if answer == num1 * num2
                                                                   then correct nums score
                                                                   else wrong nums (num1 * num2) score
                                               QuitCmd => Quit score

public export
data Fuel = Dry | More (Lazy Fuel)

public export
partial
forever : Fuel
forever = More forever

public export
run : Fuel -> ConsoleIO a -> IO (Maybe a)
run _ (Quit x) = pure (Just x)
run Dry _ = do putStrLn "Out of fuel"
               pure Nothing
run (More fuel) (Do action cont) = do res <- runCommand action
                                      run fuel (cont res)

public export
randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

public export
arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed) where
  bound : Int -> Int
  bound x with (divides x 12)
    bound ((12 * div) + rem) | (DivBy prf) = rem + 1

partial
main : IO ()
main = do seed <- time
          Just score <- run forever (quiz (arithInputs (fromInteger seed)) 0)
               | Nothing => putStrLn "Ran out of fuel"
          putStrLn ("Final score: " ++ show score)
