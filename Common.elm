module Common where

import ComputationalGeometry exposing(Point, toFloatPoint)
import Mouse

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
relativeCoordinates (x, y) = (x - size.x//2, size.y//2 - y)
