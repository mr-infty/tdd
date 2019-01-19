data SnocList : List a -> Type where
  Empty : SnocList []
  Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])

total
snocListHelper : (snoc : SnocList input) -> (rest : List a) ->
                 SnocList (input ++ rest)
snocListHelper {input} snoc [] = rewrite appendNilRightNeutral input in snoc
snocListHelper {input} snoc (x :: xs) = rewrite appendAssociative input [x] xs in
                                                snocListHelper (Snoc snoc) xs

total
snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelper {input = []} Empty xs

total
myReverseHelper : (input : List a) -> SnocList input -> List a
myReverseHelper [] Empty = []
myReverseHelper (xs ++ [x]) (Snoc rec) = x :: myReverseHelper xs rec

total
myReverse : List a -> List a
myReverse input = myReverseHelper input (snocList input)

total
myReverseWithWith : List a -> List a
myReverseWithWith input with (snocList input)
  myReverseWithWith [] | Empty = []
  myReverseWithWith (xs ++ [x]) | (Snoc rec) = x :: myReverseWithWith xs | rec

total
isSuffix : Eq a => List a -> List a -> Bool
isSuffix xs ys with (snocList xs)
  isSuffix [] ys | Empty = True
  isSuffix (xs ++ [x]) ys | (Snoc xsrec) with (snocList ys)
    isSuffix (xs ++ [x]) [] | (Snoc xsrec) | Empty = False
    isSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xsrec) | (Snoc ysrec) = case x == y of
                                                                          False => False
                                                                          True => isSuffix xs ys | xsrec | ysrec
