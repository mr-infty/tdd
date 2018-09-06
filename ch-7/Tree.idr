data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Functor Tree where
  map func Empty = Empty
  map func (Node left e right) = Node (map func left) (func e) (map func right)

Foldable Tree where
  foldr func acc Empty = acc
  foldr func acc (Node left e right) = let leftfold = foldr func acc left
                                           rightfold = foldr func leftfold right in
                                           func e rightfold
