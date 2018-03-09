module Main

main : IO ()
main = do
  putStr "Enter you name: "
  x <- getLine
  putStrLn ("Hello " ++ x ++ "!")
