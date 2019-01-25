module Exercises11_2

import InfIO

%default total

-----------------------------
-- Exercise 11.2.1
-----------------------------

totalREPL: (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action = do putStr prompt
                             res <- getLine
                             putStrLn (action res)
                             totalREPL prompt action


