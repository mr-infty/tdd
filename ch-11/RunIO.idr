module RunIO

%default total

public export
data RunIO : Type -> Type where
  Quit : a -> RunIO a
  Do : IO a -> (a -> Inf (RunIO b)) -> RunIO b

public export
(>>=) : IO a -> (a -> Inf (RunIO b)) -> RunIO b
(>>=) = Do

public export
data Fuel = Dry | More (Lazy Fuel)

partial
forever : Fuel
forever = More forever

public export
run : Fuel -> RunIO a -> IO (Maybe a)
run fuel (Quit x) = pure (Just x)
run Dry _ = do putStrLn "Out of fuel"
               pure Nothing
run (More fuel) (Do action cont) = do res <- action
                                      run fuel (cont res)

greet : RunIO ()
greet = do putStr "Enter your name: "
           name <- getLine
           if name == ""
              then do putStrLn "Bye bye!"
                      Quit ()
              else do putStrLn ("Hello " ++ show name)
                      greet

partial
main : IO ()
main = do run forever greet
          pure ()
