import Data.Vect

Position : Type
Position = (Double, Double)

Polygon : Nat -> Type
Polygon k = Vect k Position

tri : Polygon 3
tri = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)]

