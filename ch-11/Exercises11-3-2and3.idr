module Exercises11_3

%default total

-----------------------------
-- Exercise 11.3.2
-----------------------------

data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String
  ReadFile : String -> Command (Either FileError String)
  WriteFile : String -> String -> Command (Either FileError ())

  Pure : ty -> Command ty
  Bind : Command a -> (a -> Command b) -> Command b

data ConsoleIO : Type -> Type where
  Quit : a -> ConsoleIO a
  Do : Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b

namespace CommandDo
  (>>=) : Command a -> (a -> Command b) -> Command b
  (>>=) = Bind

namespace ConsoleDo
  (>>=): Command a -> (a -> Inf (ConsoleIO b)) -> ConsoleIO b
  (>>=) = Do

runCommand : Command a -> IO a
runCommand (PutStr x) = putStr x
runCommand GetLine = getLine
runCommand (ReadFile filepath) = readFile filepath
runCommand (WriteFile filepath contents) = writeFile filepath contents
runCommand (Pure x) = pure x
runCommand (Bind c f) = do res <- runCommand c
                           runCommand (f res)

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


-----------------------------
-- Exercise 11.3.2
-----------------------------

data Input = Cat String
           | Copy String String
           | Exit

parseInput : List String -> Maybe Input
parseInput ["exit"] = Just Exit
parseInput ["cat", filepath] = Just (Cat filepath)
parseInput ["copy", src, dst] = Just (Copy src dst)
parseInput _ = Nothing

mutual
  invalidInput : (prompt : String) -> ConsoleIO ()
  invalidInput prompt = do PutStr ("Invalid input.\n")
                           shell prompt

  validInput : (prompt : String) -> (input : Input) -> ConsoleIO ()
  validInput prompt (Cat filepath) = do Right contents <- ReadFile filepath
                                        | Left error => do PutStr ("File error: Could not open " ++ filepath ++ "\n")
                                                           shell prompt
                                        PutStr (contents ++ "\n")
                                        shell prompt
  validInput prompt (Copy src dst) = do Right contents <- ReadFile src
                                        | Left error => do PutStr ("File error: Could not open " ++ src ++ "\n")
                                                           shell prompt
                                        do Right _ <- WriteFile dst contents
                                           | Left error => do PutStr ("File error: Could not write to " ++ dst ++ "\n")
                                                              shell prompt
                                           PutStr ("Successfully copied " ++ src ++ " to " ++ dst ++ "\n")
                                           shell prompt
  validInput prompt Exit = Quit ()

  shell : (prompt : String) -> ConsoleIO ()
  shell prompt = do PutStr prompt
                    inputStr <- GetLine
                    case parseInput (words inputStr) of
                         Nothing => invalidInput prompt
                         Just input => validInput prompt input

partial
main : IO ()
main = do Just _ <- run forever (shell ">")
          | Nothing => putStrLn "Ran out of fuel"
          pure ()
