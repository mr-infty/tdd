module TreeLabel

%default total

public export
data Tree a = Empty
            | Node (Tree a) a (Tree a)

public export
testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred" (Node Empty "Sheila" Empty)) "Alice" (Node Empty "Bob" (Node Empty "Eve" Empty))

flatten : Tree a -> List a
flatten Empty = []
flatten (Node left val right) = flatten left ++ val :: flatten right

treeLabelWith : Stream labelType -> Tree a ->
                (Stream labelType, Tree (labelType, a))
treeLabelWith labels Empty = (labels, Empty)
treeLabelWith labels (Node left val right) = let (valLabel :: rightLabels, leftLabelled) = treeLabelWith labels left
                                                 (remainingLabels, rightLabelled) = treeLabelWith rightLabels right in
                                                 (remainingLabels, Node leftLabelled (valLabel, val) rightLabelled)

treeLabel : Stream labelType -> Tree a -> Tree (labelType, a)
treeLabel labels tree = snd (treeLabelWith labels tree)
