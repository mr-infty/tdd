import Data.Vect

readToBlank : IO (List String)
readToBlank = do x <- getLine
                 if (x == "")
                    then pure []
                    else do xs <- readToBlank
                            pure (x :: xs)
                           
listToString : List String -> String
listToString [] = ""
listToString (x :: xs) = x ++ "\n" ++ listToString xs

readAndSave : IO ()
readAndSave = do input <- readToBlank
                 filename <- getLine
                 Right _ <- writeFile filename (listToString input) | Left err => putStrLn ("Error occured while trying to write to file " ++ filename)
                 pure ()

linesInFile : (file : File) -> IO (Either FileError (n : Nat ** Vect n String))
linesInFile file = do False <- fEOF file
                      | pure (Right (_ ** []))
                      Right x <- fGetLine file
                      | Left err => pure (Left err)
                      Right (_ ** xs) <- linesInFile file
                      | Left err => pure (Left err)
                      pure (Right (_ ** x :: xs))


readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do Right file <- openFile filename Read
                           | Left err => do putStrLn ("Couldn't open file " ++ filename)
                                            pure (_ ** [])
                           Right lines <- linesInFile file | Left err => pure (_ ** [])
                           pure lines

