import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do putStr ("You guessed " ++ show guesses ++ " times. ")
                          putStr "Take a guess: "
                          Just num <- readNumber | Nothing => do putStrLn "Invalid input"
                                                                 guess target guesses
                          case compare num target of
                               LT => do putStrLn "You guessed too low!"
                                        guess target (S guesses)
                               EQ => putStrLn "You are winner!"
                               GT => do putStrLn "You guessed too high!"
                                        guess target (S guesses)

randomInt : IO (Nat)
randomInt = do secs <- time
               pure (the Nat (1 + mod (cast secs) 100))

main : IO ()
main = do target <- randomInt
          guess target 0

myRepl : String -> (String -> String) -> IO ()
myRepl prompt onInput = do putStr prompt
                           input <- getLine
                           putStr (onInput input)
                           myRepl prompt onInput

myReplWith : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()
myReplWith state prompt onInput = do putStr prompt
                                     input <- getLine
                                     case onInput state input of
                                          Just (output, newState) => do putStr output
                                                                        myReplWith newState prompt onInput
                                          Nothing => pure ()

