occurrences : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurrences item [] = 0
occurrences item (x :: xs) = case x == item of
                                  False => occurrences item xs
                                  True => 1 + occurrences item xs

data Matter = Solid | Liquid | Gas

Eq Matter where
  (==) Solid Solid = True
  (==) Liquid Liquidi = True
  (==) Gas Gas = True
  (==) _ _ = False

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Eq elem => Eq (Tree elem) where
  (==) Empty Empty = True
  (==) (Node left e right) (Node left' e' right') = left == left' && e == e' && right == right'
  (==) _ _ = False
