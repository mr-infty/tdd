printLength : IO ()
printLength = putStr "Input string: " >>= \_ =>
              getLine >>= \input => let len = length input in
                                        putStrLn (show len)

printLengthUsingDo : IO ()
printLengthUsingDo = do putStr "Input string: "
                        input <- getLine
                        let len = length input
                        putStrLn (show len)
