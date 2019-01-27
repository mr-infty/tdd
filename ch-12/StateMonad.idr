module StateMonad

import TreeLabelType

addIfPositive : Integer -> State Integer Bool
addIfPositive val = do when (val > 0) $
                            do current <- get
                               put (current + val)
                       pure (val > 0)

addPositives : List Integer -> State Integer Nat
addPositives vals = do added <- traverse addIfPositive vals
                       pure (length (filter id added))
