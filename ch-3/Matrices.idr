import Data.Vect

createEmpties : Vect m (Vect 0 a)
createEmpties = replicate _ []

transposeMat_helper : (x : Vect m a) -> (xsTrans : Vect m (Vect len a)) -> Vect m (Vect (S len) a)
transposeMat_helper [] xsTrans = []
transposeMat_helper (x :: xs) (y :: ys) = (x :: y) :: transposeMat_helper xs ys

transposeMat : Vect n (Vect m a) -> Vect m (Vect n a)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             transposeMat_helper x xsTrans
