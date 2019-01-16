module Hangman

import Data.Vect

data WordState : (guesses_remaining : Nat) -> (letters : Nat) -> Type where
  MkWordState : (word : String) ->
                (missing : Vect letters Char) ->
                WordState guesses_remaining letters

data Finished : Type where
  Won : (game : WordState (S guesses) 0) -> Finished
  Lost : (game : WordState 0 (S guesses)) -> Finished

data ValidInput : List Char -> Type where
  Letter : (c : Char) -> ValidInput [c]

Uninhabited (ValidInput []) where
  uninhabited (Letter _) impossible

Uninhabited (ValidInput (x :: (y :: xs))) where
  uninhabited (Letter _) impossible

isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput [] = No uninhabited
isValidInput (x :: []) = Yes (Letter x)
isValidInput (x :: (y :: xs)) = No uninhabited

isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput (unpack s)

readGuess : IO (x ** ValidInput x)
readGuess = do putStr "Guess:"
               s <- getLine
               case isValidString (toUpper s) of
                    Yes prf => pure (_ ** prf)
                    No contra => do putStrLn "Invalid guess"
                                    readGuess

game : WordState (S guesses) (S letters) -> IO Finished
