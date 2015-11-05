module PointInTriangle where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import List
import Debug
import Mouse
import Signal
import ComputationalGeometry as CG exposing(Point, turns)

size : { x:Int, y:Int }
size = { x = 600, y = 600 }

mouseEvents : Signal (Int, Int)
mouseEvents = Signal.sampleOn Mouse.clicks Mouse.position

inCanvas : (number, number) -> Bool
inCanvas (x, y) = x < size.x && y < size.y

-- Builds a list of coordinates from the mouse clicks,
-- starts from an empty list every four clicks.
clickCoordinates : Signal (List Point)
clickCoordinates = 
  let
    update (x, y) locations =
      if | List.length locations < 4 -> relativeCoordinates (x, y)::locations
         | otherwise -> relativeCoordinates (x,y)::[]
  in
    Signal.foldp update [] <| Signal.filter inCanvas (0,0) mouseEvents

-- Rewrite coordinates so that (0,0) is in the middle,
-- not in the upper left corner.
relativeCoordinates : (Int, Int) -> Point
relativeCoordinates (x, y) = (toFloat x - toFloat size.x/2.0, toFloat size.y/2.0 - toFloat y)

points : List Point -> List Form
points coords = (List.map (circle 2 |> filled brown |> flip move) coords)

coordPairs : List Point -> List (Point, Point)
coordPairs coords = let
    triangleCoords = List.take 3 <| List.reverse coords
  in
    case triangleCoords of
      x::xs -> List.map2 (,) (x::xs) (xs++[x])
      otherwise -> []

lines : List Point -> List Form
lines coords = List.map (\(a, b) -> outlined (solid black) (segment a b)) <| coordPairs coords

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
  

drawToCanvas lst = collage size.x size.y lst

main = Signal.map drawToCanvas
        <| Signal.map2 (++)
           (Signal.map lines clickCoordinates)
           (Signal.map points clickCoordinates)

port inTriangle : Signal (Maybe String)
port inTriangle = Signal.map pointInTriangleString clickCoordinates
