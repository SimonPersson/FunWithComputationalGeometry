module PointInTriangle where

import List as L
import Signal as S
import ComputationalGeometry as CG exposing(Point, toFloatPoint, turns
                                            , pointToForm, pointPairToLine)
import Common exposing(clickCoordinates, size)
import Graphics.Collage exposing(Form, collage)

coordinates : Signal (List Point)
coordinates = S.map (\lst -> L.take (L.length lst % 5) lst) clickCoordinates

points : List Point -> List Form
points coords = L.map pointToForm coords

coordPairs : List Point -> List (Point, Point)
coordPairs coords = let
    triangleCoords = L.take 3 <| L.reverse coords
  in
    case triangleCoords of
      x::xs -> L.map2 (,) (x::xs) (xs++[x])
      otherwise -> []

lines : List Point -> List Form
lines coords = let
    pairs = coordPairs coords
  in
    L.map pointPairToLine pairs

pointInTriangleString : List Point -> Maybe String
pointInTriangleString coords = let
    inTriangle a b c x =
      CG.turns c b a == CG.turns c b x
      && CG.turns b a c == CG.turns b a x
      && CG.turns a c b == CG.turns a c x
      && CG.turns c b a /= CG.Straight
  in
    case coords of
      x::a::b::c::_ -> if | inTriangle a b c x -> Just "Point is in triangle"
                          | otherwise -> Just "Point is not in triangle"
      otherwise -> Nothing
  
drawToCanvas lst = let
    size = Common.size
  in
    collage size.x size.y lst

main = S.map drawToCanvas
        <| S.map2 (++)
           (S.map lines coordinates)
           (S.map points coordinates)

port inTriangle : Signal (Maybe String)
port inTriangle = S.map pointInTriangleString coordinates
