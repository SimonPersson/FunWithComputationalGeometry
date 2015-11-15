module ComputationalGeometry where

import List as L
import Maybe as M
import Debug as D

type alias Point = (Int, Int)
type alias FloatPoint = (Float, Float)

type alias Line = (Point, Point)

toFloatPoint : Point -> FloatPoint
toFloatPoint (x, y) = (toFloat x, toFloat y)

type Direction = Right | Left | Straight

turns : Line -> Line -> Direction
turns ((ax, ay), (bx, by)) ((bx, by), (cx, cy)) = let
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
         | otherwise -> case turns ((px, py), (ax, ay)) ((ax, ay), (bx, by)) of
                                Right -> GT
                                Left -> LT
                                Straight -> EQ
  in
    List.sortWith comparator lst

grahamScan : List Point -> List Point -> List Line
grahamScan lst hull = let
    addPoint p h = 
      case h of
        h1::h2::hs ->
                if | turns (h1, h2) (h1, p) == Right -> p::h
                   | otherwise -> addPoint p <| L.drop 1 h
        [h1] -> p::h
        [] -> [p]
  in
    case lst of
      p::ps -> grahamScan ps <| addPoint p hull
      otherwise -> L.map2 (,) hull <| L.drop 1 <| hull ++ (L.take 1 hull)

convexHull : List Point -> List Line
convexHull points = let 
    pointOnHull = L.minimum points
    except x xs = L.filter (\a -> a/=x) xs
    sortedPoints = M.map (\poh -> poh::(sortPointsAround poh <| except poh points)) pointOnHull
  in
    case sortedPoints of
      Just ps -> grahamScan ps []
      _ -> []

intersects : Line -> Line -> Bool
intersects (a1, a2) (b1, b2) = turns (a1, a2) (a2, b1) /= turns (a1, a2) (a2, b2)
                                && turns (b1, b2) (b2, a1) /= turns (b1, b2) (b2, a2)


pointInPolygon : Point -> List Line -> Bool
pointInPolygon (px, py) l = let
    outside = (-10000, py)
    ray = ((px, py), outside)
  in
    (L.length <| L.filter (intersects ray) l) % 2 /= 0
