module InfIO

public export
data InfIO : Type where
  Do : IO a ->
       (a -> Inf InfIO)
       -> InfIO

public export
(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

total
loopPrint : String -> InfIO
loopPrint msg = Do (putStrLn msg)
                   (\_ => loopPrint msg)

public export
data Fuel = Dry | More (Lazy Fuel)

data GamingFuel = WheresMyMountainDew | EvenMore (Inf GamingFuel)

-- this is total
immer : GamingFuel
immer = EvenMore immer

-- this ain't (this does *not* contradict the info box on p. 310!)
public export
forever : Fuel
forever = More forever

tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)

public export
total
run : Fuel -> InfIO -> IO ()
run Dry _ = putStrLn "Out of fuel"
run (More fuel) (Do action cont) = do res <- action
                                      run fuel (cont res)
