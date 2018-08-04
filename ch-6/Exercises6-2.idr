import Data.Vect

Matrix : (nrows : Nat) -> (ncols : Nat) -> Type
Matrix nrows ncols = Vect nrows (Vect ncols Double)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

tupleVect : Nat -> Type -> Type
tupleVect Z ty = ()
tupleVect (S k) ty = (ty, tupleVect k ty)

test : tupleVect 4 Nat
test = (1, 2, 3, 4, ())

