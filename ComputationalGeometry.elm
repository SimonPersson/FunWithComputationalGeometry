module ComputationalGeometry where

import Color exposing (brown, black)
import Graphics.Collage exposing (Form, path, segment, circle, move, filled
                                  , solid, outlined)

type alias Point = (Int, Int)
type alias FloatPoint = (Float, Float)

toFloatPoint : Point -> FloatPoint
toFloatPoint (x, y) = (toFloat x, toFloat y)

type Direction = Right | Left | Straight

turns : Point -> Point -> Point -> Direction
turns (ax, ay) (bx, by) (cx, cy) = let
    cross = (ax-bx)*(by-cy)-(ay-by)*(bx-cx)
  in 
    if | cross < 0 -> Right
       | cross > 0 -> Left
       | otherwise -> Straight
 
sortPointsAround : Point -> List Point -> List Point
sortPointsAround (px, py) lst = let
    comparator (ax, ay) (bx, by) = 
      if | ax < px && bx >= px -> LT
         | ax >= px && bx < px -> GT
         | otherwise -> case turns (px, py) (ax, ay) (bx, by) of
                                Right -> GT
                                Left -> LT
                                Straight -> EQ
  in
    List.sortWith comparator lst

sortedPath : List Point -> List Form
sortedPath lst = let
    rev = List.reverse lst
  in
    case rev of
      p::ps -> [outlined (solid black) <| path
                                       <| List.map toFloatPoint
                                       <| sortPointsAround p ps
                                       ]
      otherwise -> []

pointToForm : Point -> Form
pointToForm coords = move (toFloatPoint coords) <| filled brown <| circle 2

pointPairToLine : (Point, Point) -> Form
pointPairToLine (a, b) = outlined (solid black) <| segment (toFloatPoint a) (toFloatPoint b)
