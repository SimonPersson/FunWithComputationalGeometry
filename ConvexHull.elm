module ConvexHull where

import Graphics.Collage exposing (Form, collage, outlined)
import List as L
import Signal as S
import ComputationalGeometry exposing(Point, toFloatPoint, Line, convexHull)
import Common exposing (clickCoordinates, size, pointToForm, lineToForm)

points : List Point -> List Form
points coords = List.map pointToForm coords

drawToCanvas lst = collage size.x size.y lst

main = Signal.map drawToCanvas <|
       Signal.map2 (++)
       (Signal.map (\x -> L.map lineToForm <| convexHull x) clickCoordinates)
       (Signal.map points clickCoordinates)
