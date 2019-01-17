module RemoveElem

import Data.Vect

myRemoveElem : (value : a) ->
             (xs : Vect (S n) a) ->
             (prf : Elem value xs) ->
             Vect n a
myRemoveElem value (value :: ys) Here = ys
myRemoveElem value (y :: []) (There later) = []
myRemoveElem value (y :: (x :: xs)) (There later) = y :: (myRemoveElem value (x :: xs) later)

export
removeElem : (value : a) ->
             (xs : Vect (S n) a) ->
             (prf : Elem value xs) ->
             Vect n a
removeElem value (value :: ys) Here = ys
removeElem {n = Z} value (y :: []) (There later) = absurd later
removeElem {n = (S k)} value (y :: ys) (There later) = y :: removeElem value ys later

removeElem_auto : (value : a) ->
             (xs : Vect (S n) a) ->
             {auto prf : Elem value xs} ->
             Vect n a
removeElem_auto value (value :: ys) {prf = Here} = ys
removeElem_auto {n = Z} value (y :: []) {prf = (There later)} = absurd later
removeElem_auto {n = (S k)} value (y :: ys) {prf = (There later)} = y :: removeElem_auto value ys {prf = later}
