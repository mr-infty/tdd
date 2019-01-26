module TreeLabelState

import Control.Monad.State
import TreeLabel

treeLabelWith : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWith Empty = pure Empty
treeLabelWith (Node left val right) = do left_labelled <- treeLabelWith left
                                         (this :: rest) <- get
                                         put rest
                                         right_labelled <- treeLabelWith right
                                         pure (Node left_labelled (this, val) right_labelled)

treeLabel : Tree a -> Stream labelType -> Tree (labelType, a)
treeLabel tree labels = evalState (treeLabelWith tree) labels
