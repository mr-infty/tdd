module WordCount

import ProcessLib

record WCData where
  constructor MkWCData
  wordCount : Nat
  lineCount : Nat

doCount : String -> WCData
doCount str = let wc = length (words str)
                  lc = length (lines str) in
                  MkWCData wc lc

data WC = CountFile String
        | GetData String

WCType : WC -> Type
WCType (CountFile fname) = ()
WCType (GetData fname) = Maybe WCData

countFile : (fname : String) -> (loaded : List (String, WCData)) -> Process WCType (List (String, WCData)) Sent Sent
countFile fname loaded = do Right content <- Action (readFile fname)
                            | Left error => Pure loaded
                            let count = doCount content -- TODO: Why not "in" here?!?
                            Action (putStrLn ("Counting complete for " ++ show fname))
                            Pure ((fname, count) :: loaded)

wcService : (loaded : List (String, WCData)) ->
            Service WCType ()
wcService loaded = do msg <- Respond (\msg => case msg of
                                                   CountFile fname => Pure ()
                                                   GetData fname => Pure (lookup fname loaded))
                      newLoaded <- case msg of
                                        Just (CountFile fname) => countFile fname loaded
                                        _ => Pure loaded
                      Loop (wcService newLoaded)

procMain : Client ()
procMain = do Just wc <- Spawn (wcService [])
              | Nothing => Action (putStrLn "Spawn failed")
              Action (putStrLn "Counting test.txt")
              Request wc (CountFile "test.txt")

              Action (putStrLn "Processing")
              Just wcdata <- Request wc (GetData "test.txt")
              | Nothing => Action (putStrLn "File error")
              Action (putStrLn ("Words: " ++ show (wordCount wcdata)))
              Action (putStrLn ("Lines: " ++ show (lineCount wcdata)))
