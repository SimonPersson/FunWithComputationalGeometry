module RadialSort where

import Graphics.Collage exposing (Form, collage, outlined)
import List as L
import Maybe as M
import Signal as S
import ComputationalGeometry as CG exposing(Point, toFloatPoint)
import Common exposing (clickCoordinates, size, pointToForm, sortedPathAround)

points : List Point -> List Form
points coords = L.map pointToForm coords

drawToCanvas lst = collage size.x size.y lst

main = S.map drawToCanvas <|
         S.map2 (++)
         (S.map (\ps -> sortedPathAround (M.withDefault (0,0) <| L.head ps)
                                         (L.drop 1 ps)
                ) clickCoordinates)
         (S.map points clickCoordinates)
