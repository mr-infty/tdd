import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

guess : (target : Nat) -> IO ()
guess target = do putStr "Take a guess: "
                  Just num <- readNumber | Nothing => do putStrLn "Invalid input"
                                                         guess target
                  case compare num target of
                       LT => do putStrLn "You guessed too low!"
                                guess target
                       EQ => putStrLn "You are winner!"
                       GT => do putStrLn "You guessed too high!"
                                guess target

randomInt : IO (Nat)
randomInt = do secs <- time
               pure (the Nat (1 + mod (cast secs) 100))

main : IO ()
main = do target <- randomInt
          guess target
