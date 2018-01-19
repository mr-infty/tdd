twice : (a -> a) -> a -> a
twice f x = f (f x)

Shape : Type
rotate : Shape -> Shape

double : Num a => a -> a
double x = x + x

quadruple : Num a => a -> a
quadruple = twice double

turnAround : Shape -> Shape
turnAround = twice rotate
