module Common where

import ComputationalGeometry exposing(Point, Line, toFloatPoint, sortPointsAround)
import Mouse
import Color exposing (brown, black)
import Graphics.Collage exposing (Form, path, segment, circle, move, filled
                                  , solid, outlined)

size : { x:Int, y:Int }
size = { x = 600, y = 600 }

mouseEvents : Signal Point
mouseEvents = Signal.sampleOn Mouse.clicks Mouse.position

inCanvas : Point -> Bool
inCanvas (x, y) = x < size.x && y < size.y

-- Builds a list of coordinates from the mouse clicks,
-- starts from an empty list every four clicks.
clickCoordinates : Signal (List Point)
clickCoordinates = 
  let
    update (x, y) locations = locations++[relativeCoordinates (x, y)]
  in
    Signal.foldp update [] <| Signal.filter inCanvas (0,0) mouseEvents

-- Rewrite coordinates so that (0,0) is in the middle,
-- not in the upper left corner.
relativeCoordinates : Point -> Point
relativeCoordinates (x, y) = (x - size.x//2, size.y//2 - y)

sortedPathAround : Point -> List Point -> List Form
sortedPathAround p lst = [outlined (solid black) <| path
                       <| List.map toFloatPoint
                       <| sortPointsAround p lst
                   ]

pointToForm : Point -> Form
pointToForm coords = move (toFloatPoint coords) <| filled brown <| circle 2

pointToBigForm : Point -> Form
pointToBigForm coords = move (toFloatPoint coords) <| filled black <| circle 5

lineToForm : Line -> Form
lineToForm (a, b) = outlined (solid black) <| segment (toFloatPoint a) (toFloatPoint b)
