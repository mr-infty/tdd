module DescribeList

describeList : List Int -> String
describeList [] = "Empty"
describeList (x :: xs) = "Non-empty, tail = " ++ show xs

data ListLast : List a -> Type where
  Empty : ListLast []
  NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

listLast : (input : List a) -> ListLast input
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                          Empty => NonEmpty [] x
                          NonEmpty xs y => NonEmpty ([x] ++ xs) y 

describeListHelper : Show a => (input : List a) -> (form : ListLast input) -> String
describeListHelper [] Empty = "Empty"
describeListHelper (xs ++ [x]) (NonEmpty xs x) = "Non-empty, initial portion = " ++ show xs

describeListEndNaive : List Int -> String
describeListEndNaive input = describeListHelper input (listLast input)

describeListEnd : List Int -> String
describeListEnd input with (listLast input)
  describeListEnd [] | Empty = "Empty"
  describeListEnd (xs ++ [x]) | (NonEmpty xs x) = "Non-empty, initial portion = " ++ show xs

myReverse : List a -> List a
myReverse input with (listLast input)
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | (NonEmpty xs x) = x :: myReverse xs
