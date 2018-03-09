printLonger : IO ()
printLonger = do putStr "First string: "
                 str1 <- getLine
                 putStr "Second string: "
                 str2 <- getLine
                 let len1 = length str1
                 let len2 = length str2
                 let longerLength = if len1 < len2 then len2 else len1
                 putStrLn (show longerLength)

printLongerWithoutDo : IO ()
printLongerWithoutDo = putStr "First string: " >>= \_ =>
                       getLine >>= \str1 =>
                                   putStr "Second string: " >>= \_ =>
                                                          getLine >>= \str2 =>
                                                                      let len1 = length str1 in
                                                                          let len2 = length str2 in
                                                                              putStrLn (show (if len1 < len2 then len2 else len1))

