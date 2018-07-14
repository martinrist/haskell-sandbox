module RealWorld.Chapter03.ShapeUnion where

type Vector = (Double, Double)

data Shape
    = Circle Vector
             Double
    | Poly [Vector]