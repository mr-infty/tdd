import Data.Vect

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse {n = S k} (x :: xs) = let result = myReverse xs ++ [x] in
                                    rewrite plusCommutative 1 k in result

reverseProof : Vect (len + 1) elem -> Vect (S len) elem
reverseProof {len} result = rewrite plusCommutative 1 len in result

myReverse2 : Vect n elem -> Vect n elem
myReverse2 [] = []
myReverse2 (x :: xs) = reverseProof (myReverse xs ++ [x])

test1 : Vect 4 Int
test1 = [1, 2, 3, 4]

test2 : Vect (2+2) Int
test2 = test1
