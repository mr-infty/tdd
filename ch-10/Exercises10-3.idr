module Exercises10_3

import DataStore
import Shape_abs

-----------------------------
-- Exercise 10.3.1
-----------------------------

getValues : DataStore (SString .+. val_schema) ->
            List (SchemaType val_schema)
getValues store with (storeView store)
  getValues store | SNil = []
  getValues (addToStore (key, value) store) | (SAdd rec) = value :: getValues store | rec

testStore : DataStore (SString .+. SInt)
testStore = addToStore ("First", 1) $
            addToStore ("Second", 2) $
            empty
            
-----------------------------
-- Exercise 10.3.2
-----------------------------

area : Shape -> Double
area s with (shapeView s)
  area (triangle base height) | (STriangle base height) = 0.5 * base * height
  area (rectangle width height) | (SRectangle width height) = width * height
  area (circle radius) | (SCircle radius) = pi * radius * radius
