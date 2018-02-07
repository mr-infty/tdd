allLengths : List String -> List Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words

isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not (isEven k)
