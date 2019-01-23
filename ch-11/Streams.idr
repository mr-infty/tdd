module Streams

total
labelWith : Stream labelType -> List a -> List (labelType, a)
labelWith lbs [] = []
labelWith (lbl :: lbls) (val :: vals) = (lbl, val) :: labelWith lbls vals


total
label : List a -> List (Integer, a)
label = labelWith [0..]
