module Exercises11_3

import ArithCmdDo
import System

-----------------------------
-- Exercise 11.3.1
-----------------------------

namespace Ex_1
  mutual
    correct : Stream Int -> (score : Nat) -> (tries : Nat) -> ConsoleIO (Nat, Nat)
    correct nums score tries = do PutStr "Correct!\n"
                                  quiz nums (score + 1) (tries + 1)

    wrong : Stream Int -> Int -> (score : Nat) -> (tries : Nat) -> ConsoleIO (Nat, Nat)
    wrong nums answer score tries = do PutStr ("Wrong, the answer is " ++ show answer ++ "\n")
                                       quiz nums score (tries + 1)

    quiz : Stream Int -> (score : Nat) -> (tries : Nat) -> ConsoleIO (Nat, Nat)
    quiz (num1 :: num2 :: nums) score tries = do PutStr ("Score so far: " ++ show score ++ " / " ++ show tries ++ "\n")
                                                 input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
                                                 case input of
                                                      Answer answer => if answer == num1 * num2
                                                                          then correct nums score tries
                                                                          else wrong nums (num1 * num2) score tries
                                                      QuitCmd => Quit (score, tries)

  partial
  main : IO ()
  main = do seed <- time
            Just (score, tries) <- run forever (quiz (arithInputs (fromInteger seed)) 0 0)
                 | Nothing => putStrLn "Ran out of fuel"
            putStrLn ("Final score: " ++ show score ++ " / " ++ show tries)

