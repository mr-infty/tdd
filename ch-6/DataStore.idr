module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SChar
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SChar = Char
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size items) newitem = MkData schema _ (addToData items)
   where
     addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
     addToData [] = [newitem]
     addToData (x :: xs) = x :: addToData xs

data Command : Schema -> Type where
  SetSchema : (newschema : Schema) -> Command schema
  Add : SchemaType schema -> Command schema
  Get : Maybe Integer -> Command schema
  Size : Command schema
  Search : String -> Command schema
  Quit : Command schema

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input) where
  getQuoted : List Char -> Maybe (String, String)
  getQuoted ('"' :: xs) = case span (/= '"') xs of
                               (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                               _ => Nothing
  getQuoted _ = Nothing
parsePrefix SChar input = getChar (unpack input) where
  getChar : List Char -> Maybe (Char, String)
  getChar [] = Nothing
  getChar (x :: xs) = Just (x, ltrim (pack xs))
parsePrefix SInt input = case span isDigit input of
                              ("", post) => Nothing
                              (pre, post) => Just (cast pre, ltrim post)
parsePrefix (schemal .+. schemar) input = do (lval, input') <- parsePrefix schemal input
                                             (rval, input'') <- parsePrefix schemar input'
                                             Just ((lval, rval), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Nothing => Nothing
                                  Just (val, "") => Just val
                                  Just (val, rest) => Nothing

parseElementarySchema : String -> Maybe Schema
parseElementarySchema "String" = Just SString
parseElementarySchema "Char" = Just SChar
parseElementarySchema "Int" = Just SInt
parseElementarySchema _ = Nothing


parseSchema : List String -> Maybe Schema
parseSchema (x :: xs) = do schemal <- parseElementarySchema x
                           case xs of
                                [] => Just schemal
                                _ => do schemar <- parseSchema xs
                                        Just (schemal .+. schemar)
parseSchema [] = Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "schema" str = case parseSchema (words str) of
                                        Just newschema => Just (SetSchema newschema)
                                        Nothing => Nothing
parseCommand schema "add" str = case parseBySchema schema str of
                                     Nothing => Nothing
                                     (Just val) => Just (Add val)
parseCommand schema "get" val = case all isDigit (unpack val) of
                              True => if val == ""
                                         then Just (Get Nothing)
                                         else Just (Get (Just (cast val)))
                              False => Nothing
parseCommand schema "size" "" = Just Size
parseCommand schema "search" str = Just (Search str)
parseCommand schema "quit" "" = Just Quit
parseCommand schema _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                          (cmd, args) => parseCommand schema cmd (ltrim args)

display : SchemaType schema -> String
display {schema = SString} x = x
display {schema = SChar} x = show x
display {schema = SInt} x = show x
display {schema = (y .+. z)} (a, b) = display a ++ ", " ++ display b

showSearchResults : (items : Vect size (SchemaType schema)) -> (indices : List (Fin size)) -> String
showSearchResults items indices = case indices of
                                       [] => "No hits\n"
                                       _ => foldl (++) "" (map (\idx => show (finToInteger idx) ++ ": " ++ display (index idx items) ++ "\n") indices)

indices : (n : Nat) -> List (Fin n)
indices Z = []
indices (S k) = 0 :: map FS (indices k)

searchVect : (str : String) -> (items : Vect size (SchemaType schema)) -> List (Fin size)
searchVect {size} str items = filter (\idx => isInfixOf str (display (index idx items))) (indices size)

setSchema : (store : DataStore) -> (newschema : Schema) -> Maybe DataStore
setSchema store newschema = case size store of
                                 Z => Just (MkData newschema 0 [])
                                 (S k) => Nothing

getAllEntries : (store : DataStore) -> String
getAllEntries store = foldl (++) "" (map (\idx => show (finToInteger idx) ++ ": " ++ display (index idx (items store)) ++ "\n") (indices (size store)))

getEntry : (maybeId : Maybe Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry maybeId store = case maybeId of
                              Nothing => Just (getAllEntries store, store)
                              Just id => case integerToFin id (size store) of
                                              Nothing => Just ("Out of range\n", store)
                                              Just idx => Just (display (index idx (items store)) ++ "\n", store)

processCommand : (store : DataStore) -> (Command (schema store)) -> Maybe (String, DataStore)
processCommand store cmd = case cmd of
                                SetSchema newschema => (case setSchema store newschema of
                                                             Nothing => Just ("Can't update schema\n", store)
                                                             (Just store') => Just ("OK\n", store'))
                                Add item => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                                Get maybeId => getEntry maybeId store
                                Size => Just (show (size store) ++ "\n", store)
                                Search str => Just (showSearchResults (items store) (searchVect str (items store)), store)
                                Quit => Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse (schema store) input of
                                Just cmd => processCommand store cmd
                                Nothing => Just ("Invalid command\n", store)

main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput
