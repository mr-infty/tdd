||| Given a string, returns true iff it is a palindrome.
palindrome : (str : String) -> Bool
palindrome str = str == (reverse str)

||| Given a string, returns true iff it is the same backwards
||| and forwards, ignoring case.
palindromeCaseInsensitive : String -> Bool
palindromeCaseInsensitive str = palindrome (toLower str)

||| Given a string, return true iff it is a palindrome and longer
||| than 10 characters.
nontrivialPalindrome : String -> Bool
nontrivialPalindrome str = (length str) > 10 && (palindrome str)

palindromeLongerThan : Nat -> String -> Bool
palindromeLongerThan n str = (length str) > n && palindrome str

||| Given a string, returns the number of words and number of characters in it.
counts : (str : String) -> (Nat, Nat)
counts str = (length (words str), length str)

topTen : Ord a => List a -> List a
topTen list = take 10 (reverse (sort list))

over_length : Nat -> List String -> List String
over_length n list = filter (\x => length x > n) list
