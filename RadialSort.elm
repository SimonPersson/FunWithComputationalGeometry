module RadialSort where

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
    update (x, y) locations = relativeCoordinates (x, y)::locations
  in
    Signal.foldp update [] <| Signal.filter inCanvas (0,0) mouseEvents

-- Rewrite coordinates so that (0,0) is in the middle,
-- not in the upper left corner.
relativeCoordinates : (Int, Int) -> Point
relativeCoordinates (x, y) = (toFloat x - toFloat size.x/2.0, toFloat size.y/2.0 - toFloat y)

points : List Point -> List Form
points coords = (List.map (circle 2 |> filled brown |> flip move) coords)

sortPointsAround : Point -> List Point -> List Point
sortPointsAround (px, py) lst = let
    comparator (ax, ay) (bx, by) = 
      if | ax < px && bx >= px -> LT
         | ax >= px && bx < px -> GT
         | otherwise -> case CG.turns (px, py) (ax, ay) (bx, by) of
                                CG.Right -> GT
                                CG.Left -> LT
                                CG.Straight -> EQ
  in
    List.sortWith comparator lst

sortedPath : List Point -> List Form
sortedPath lst = let
    rev = List.reverse lst
  in
    case rev of
      p::ps -> [outlined (solid black) <| path <| sortPointsAround p ps]
      otherwise -> []

drawToCanvas lst = collage size.x size.y lst

main = Signal.map drawToCanvas <|
       Signal.map2 (++)
       (Signal.map sortedPath clickCoordinates)
       (Signal.map points clickCoordinates)
