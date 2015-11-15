module PointInPolygon where

import Graphics.Collage exposing (Form, collage, outlined)
import List as L
import Maybe as M
import Signal as S
import ComputationalGeometry exposing(Point, toFloatPoint, Line, convexHull, pointInPolygon)
import Common exposing (clickCoordinates, size, pointToForm, sortedPathAround, lineToForm, pointToBigForm)

constPoint : Point
constPoint = (250, 250)

points : List Point -> List Form
points coords = List.map pointToForm coords

lines : List Line -> List Form
lines l = List.map lineToForm l

drawToCanvas lst = collage size.x size.y lst

polygonFromPoints pts = L.map2 (,) pts <| (L.drop 1 pts) ++ (L.take 1 pts)

main = Signal.map drawToCanvas <|
       Signal.map (\l -> pointToBigForm constPoint::l) <|
       Signal.map2 (++)
       (Signal.map (\xs -> L.map lineToForm <| polygonFromPoints xs) clickCoordinates)
       (Signal.map points clickCoordinates)


port inPolygon : Signal Bool
port inPolygon = S.map (\x -> pointInPolygon constPoint <| polygonFromPoints x) clickCoordinates
