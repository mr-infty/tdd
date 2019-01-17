module Hangman

import Data.Vect
import RemoveElem

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

processGuess : (letter : Char) ->
               WordState (S guesses) (S letters) ->
               Either (WordState guesses (S letters))
                      (WordState (S guesses) letters)
processGuess letter (MkWordState word missing) = case isElem letter missing of
                                                      Yes prf => Right (MkWordState word (removeElem letter missing prf))
                                                      No contra => Left (MkWordState word missing)

game : WordState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} st = do (_ ** Letter letter) <- readGuess
                                 case processGuess letter st of
                                      Left l => do putStrLn "Wrong!"
                                                   case guesses of
                                                        Z => pure (Lost l)
                                                        (S k) => game l
                                      Right r => do putStrLn "Right!"
                                                    case letters of
                                                         Z => pure (Won r)
                                                         (S k) => game r

main : IO ()
main = do result <- game {guesses = 2}
                         (MkWordState "Test" ['T', 'E', 'S'])
          case result of
               Won game => putStrLn "You're winner!"
               Lost (MkWordState word missing) => putStrLn ("You're loser. The word was " ++ word)
