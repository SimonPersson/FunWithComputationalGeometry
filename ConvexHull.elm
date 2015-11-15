module ConvexHull where

import Graphics.Collage exposing (Form, collage, outlined)
import List as L
import Signal as S
import ComputationalGeometry exposing(Point, pointToForm, toFloatPoint, sortedPath, Line, lineToForm, convexHull)
import Common exposing (clickCoordinates, size)

points : List Point -> List Form
points coords = List.map pointToForm coords

lines : List Line -> List Form
lines l = List.map lineToForm l

drawToCanvas lst = collage size.x size.y lst

main = Signal.map drawToCanvas <|
       Signal.map2 (++)
       (Signal.map (\x -> L.map lineToForm <| convexHull x) clickCoordinates)
       (Signal.map points clickCoordinates)
