module Traverse

crew : List String
crew = ["Lister", "Rimmer", "Kryten", "Cat"] --TODO: Watch "Red Dwarf"

main : IO ()
main = do putStr "Display Crew? "
          x <- getLine
          when (x == "yes") $
                    do traverse putStrLn crew
                       pure ()
          putStrLn "Done"
