module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
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
  Get : Integer -> Command schema
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
parsePrefix SInt input = case span isDigit input of
                              ("", post) => Nothing
                              (pre, post) => Just (cast pre, ltrim post)
parsePrefix (schemal .+. schemar) input = case parsePrefix schemal input of
                                               (Just (lval, input')) => (case parsePrefix schemar input' of
                                                                              Just (rval, input'') => Just ((lval, rval), input'')
                                                                              Nothing => Nothing)
                                               Nothing => Nothing

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Nothing => Nothing
                                  Just (val, "") => Just val
                                  Just (val, rest) => Nothing

parseSchema : List String -> Maybe Schema
parseSchema ["String"] = Just SString
parseSchema ["Int"] = Just SInt
parseSchema (x :: xs) = case parseSchema [x] of
                             Nothing => Nothing
                             Just schemal => case parseSchema xs of
                                                  Nothing => Nothing
                                                  Just schemar => Just (schemal .+. schemar)
parseSchema _ = Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "schema" str = case parseSchema (words str) of
                                        Just newschema => Just (SetSchema newschema)
                                        Nothing => Nothing
parseCommand schema "add" str = case parseBySchema schema str of
                                     Nothing => Nothing
                                     (Just val) => Just (Add val)
parseCommand schema "get" val = case all isDigit (unpack val) of
                              True => Just (Get (cast val))
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
display {schema = SInt} x = show x
display {schema = (y .+. z)} (a, b) = display a ++ ", " ++ display b

showSearchResults : (items : Vect size (SchemaType schema)) -> (indices : List (Fin size)) -> String
showSearchResults items indices = case indices of
                                       [] => "No hits\n"
                                       _ => foldl (++) "" (map (\idx => show (finToInteger idx) ++ ": " ++ display (index idx items) ++ "\n") indices)

searchVect : (str : String) -> (items : Vect size (SchemaType schema)) -> List (Fin size)
searchVect {size} str items = filter (\idx => isInfixOf str (display (index idx items))) (indices size)
   where
     indices : (n : Nat) -> List (Fin n)
     indices Z = []
     indices (S k) = 0 :: map FS (indices k)

setSchema : (store : DataStore) -> (newschema : Schema) -> Maybe DataStore
setSchema store newschema = case size store of
                                 Z => Just (MkData newschema 0 [])
                                 (S k) => Nothing

processCommand : (store : DataStore) -> (Command (schema store)) -> Maybe (String, DataStore)
processCommand store cmd = case cmd of
                                SetSchema newschema => (case setSchema store newschema of
                                                             Nothing => Just ("Can't update schema\n", store)
                                                             (Just store') => Just ("OK\n", store'))
                                Add item => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                                Get id => case integerToFin id (size store) of
                                                 Nothing => Just ("Out of range\n", store)
                                                 Just idx => Just (display (index idx (items store)) ++ "\n", store)
                                Size => Just (show (size store) ++ "\n", store)
                                Search str => Just (showSearchResults (items store) (searchVect str (items store)), store)
                                Quit => Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse (schema store) input of
                                Just cmd => processCommand store cmd
                                Nothing => Just ("Invalid command\n", store)

main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput
