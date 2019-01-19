module Shape_abs

export
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

export
triangle : Double -> Double -> Shape
triangle = Triangle

export
rectangle : Double -> Double -> Shape
rectangle = Rectangle

export
circle : Double -> Shape
circle = Circle

public export
data ShapeView : (shape : Shape) -> Type where
  STriangle : (base : Double) -> (height : Double) -> ShapeView (triangle base height)
  SRectangle : (width : Double) -> (height : Double) -> ShapeView (rectangle width height)
  SCircle : (radius : Double) -> ShapeView (circle radius)

export
shapeView : (shape : Shape) -> ShapeView shape
shapeView (Triangle base height) = STriangle base height
shapeView (Rectangle width height) = SRectangle width height
shapeView (Circle radius) = SCircle radius
