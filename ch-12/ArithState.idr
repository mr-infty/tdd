module ArithState

import Data.Primitives.Views
import System

%default total

public export
record Score where
  constructor MkScore
  correct : Nat
  attempted : Nat

public export
Show Score where
  show score = show (correct score) ++ "/" ++
               show (attempted score) ++ "\n"

public export
record GameState where
  constructor MkGameState
  score : Score
  difficulty : Int

public export
Show GameState where
  show st = show (score st) ++
            "Difficulty: " ++ show (difficulty st)

setDifficulty : Int -> GameState -> GameState
setDifficulty newDiff = record { difficulty = newDiff }

addWrong : GameState -> GameState
--addWrong state = record { score->attempted = attempted (score state) + 1 } state
addWrong = record { score->attempted $= (+1) }


addCorrect : GameState -> GameState
--addCorrect state = record { score->correct = correct (score state) + 1,
--                            score->attempted = attempted (score state) + 1 } state
addCorrect = record { score->correct $= (+1),
                      score->attempted $= (+1) }

initState : GameState
initState = MkGameState (MkScore 0 0) 12

public export
data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String

  GetRandom : Command Int
  GetGameState : Command GameState
  PutGameState : GameState -> Command ()

  Pure : ty -> Command ty
  Bind : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace CommandDo
  public export
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleDo
  (>>=): Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

runCommand : Stream Int ->
             GameState ->
             Command a -> IO (a, Stream Int, GameState)
runCommand rnds state (PutStr x) = do putStr x
                                      pure ((), rnds, state)
runCommand rnds state GetLine = do line <- getLine
                                   pure (line, rnds, state)
runCommand rnds state (Pure x) = pure (x, rnds, state)
runCommand rnds state (Bind c f) = do (res, newRnds, newState) <- runCommand rnds state c
                                      runCommand newRnds newState (f res)
runCommand rnds state (PutGameState newState) = pure ((), rnds, newState)
runCommand rnds state GetGameState = pure (state, rnds, state)
runCommand (rnd :: rnds) state GetRandom = pure (rnd, rnds, state)

data Input = Answer Int
           | QuitCmd

readInput : (prompt : String) -> Command Input
readInput prompt = do PutStr prompt
                      answer <- GetLine
                      if toLower answer == "quit"
                         then Pure QuitCmd
                         else Pure (Answer (cast answer))

mutual
  correct : ConsoleIO GameState
  correct = do st <- GetGameState
               PutGameState (addCorrect st)
               quiz

  wrong : Int -> ConsoleIO GameState
  wrong answer = do PutStr ("Wrong, the answer is " ++ show answer ++ "\n")
                    st <- GetGameState
                    PutGameState (addWrong st)
                    quiz

  quiz : ConsoleIO GameState
  quiz = do num1 <- GetRandom
            num2 <- GetRandom
            st <- GetGameState
            PutStr (show st ++ "\n")

            input <- readInput (show num1 ++ " * " ++ show num2 ++ "? ")
            case input of
                 Answer answer => if answer == num1 * num2
                                     then correct
                                     else wrong (num1 * num2)
                 QuitCmd => Quit st

data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

run : Fuel -> Stream Int -> GameState -> ConsoleIO a ->
      IO (Maybe a, Stream Int, GameState)
run _ rnds state (Quit x) = pure (Just x, rnds, state)
run Dry rnds state _ = do putStrLn "Out of fuel"
                          pure (Nothing, rnds, state)
run (More fuel) rnds state (Do action cont) = do (res, newRnds, newState) <- runCommand rnds state action
                                                 run fuel newRnds newState (cont res)

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

arithInputs : Int -> Stream Int
arithInputs seed = map bound (randoms seed) where
  bound : Int -> Int
  bound x with (divides x 12)
    bound ((12 * div) + rem) | (DivBy prf) = rem + 1

partial
main : IO ()
main = do seed <- time
          (Just endState, _, _) <- run forever (arithInputs (fromInteger seed)) initState quiz
               | (Nothing, _, _) => putStrLn "Ran out of fuel"
          putStrLn ("Final score: " ++ show (score endState))
