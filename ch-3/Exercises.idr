import Data.Vect

my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

my_append : (xs : List a) -> (x : a) -> List a
my_append [] x = [x]
my_append (y :: xs) x = y :: my_append xs x

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = my_append (my_reverse xs) x

my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

my_map2 : (a -> b) -> Vect n a -> Vect n b
my_map2 f [] = []
my_map2 f (x :: xs) = f x :: my_map2 f xs

createEmpties : Vect m (Vect 0 a)
createEmpties {m = Z} = []
createEmpties {m = (S k)} = [] :: createEmpties

{- Exercise 1 -}
transposeMat : Vect n (Vect m a) -> Vect m (Vect n a)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             zipWith (::) x xsTrans

{- Exercise 2 -}
addVector : Num a => Vect m a -> Vect m a -> Vect m a
addVector xs ys = zipWith (+) xs ys 

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix xs ys = zipWith addVector xs ys

dotProd : Num a => Vect k a -> Vect k a -> a
dotProd [] ys = 0
dotProd (x :: xs) (y :: ys) = x*y + dotProd xs ys

{- Exercise 3 -}
multMatrix : Num a => Vect n (Vect k a) -> Vect k (Vect m a) -> Vect n (Vect m a)
multMatrix xs ys = let ysTrans = transposeMat ys in
                       map (\x => map (dotProd x) ysTrans) xs
