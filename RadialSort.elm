module RadialSort where

import Graphics.Collage exposing (Form, collage, outlined)
import List as L
import Signal as S
import ComputationalGeometry as CG exposing(Point, pointToForm, turns
                                            , toFloatPoint, sortedPath)
import Common exposing (clickCoordinates, size)

points : List Point -> List Form
points coords = List.map pointToForm coords

drawToCanvas lst = collage size.x size.y lst

main = Signal.map drawToCanvas <|
       Signal.map2 (++)
       (Signal.map sortedPath clickCoordinates)
       (Signal.map points clickCoordinates)
