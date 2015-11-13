module RadialSort where

import Graphics.Collage exposing (Form, collage, outlined)
import List as L
import Signal as S
import ComputationalGeometry as CG exposing(Point, pointToForm, toFloatPoint, sortedPath)
import Common exposing (clickCoordinates, size)

points : List Point -> List Form
points coords = L.map pointToForm coords

drawToCanvas lst = collage size.x size.y lst

main = S.map drawToCanvas <|
       S.map2 (++)
       (S.map sortedPath clickCoordinates)
       (S.map points clickCoordinates)
