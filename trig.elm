import Graphics.Collage exposing (..)
import Color exposing (..)
import Window exposing (dimensions)
import Graphics.Element exposing (..)
import Signal exposing (..)
import Mouse exposing (..)
import Text exposing (..)
import String exposing (dropRight, right)
-- import Math exposing (..)

main : Signal Element
main =
  let dimensify fn wdim mPos = fn (fst wdim) (snd wdim) (grf wdim mPos)
      grf : (Int, Int) -> (Int, Int) -> (Float, Float)
      grf (w, h) (x, y) = (toFloat x - (toFloat w)/2, (toFloat h)/2 - toFloat y)
  in
    map2 (dimensify view) Window.dimensions Mouse.position

view : Int -> Int -> (Float, Float) -> Element
view w h mousePos =
  let
    x = fst mousePos
    y = snd mousePos
    angle' = atan2 y x
    angle = if angle' < 0 then angle' + 2 * pi else angle'
  in
    collage w h [
      circle radius |> outlined { thickLine | color = grey }
    
      -- horizontal cross bar
    , segment (-radius, 0) (radius, 0) |> traced { defaultLine | color = grey }
  
  
      -- triangle legs
    , segment (0,0) (radius * (cos angle), 0) |> traced defaultLine
    , segment (radius * (cos angle),0)
              (radius * (cos angle), radius * (sin angle))
          |> traced defaultLine
    , segment (0,0) (radius * (cos angle), radius * (sin angle))
          |> traced defaultLine
    
      -- mouse line
    , segment (0,0) mousePos |> traced { thickLine | color = blue, width = 8 }
    
      -- angle text
    , move (radius + 84, 20)
          <| text
          <| fromString
          <| "angle: " ++ (precision 2 angle)
             ++ " (" ++ (precision 2 (angle / pi)) ++ "\x22c5\&π)"
             ++ " radians"
  ]

-- Util

precision : Int -> Float -> String
precision p num =
  let
    num' : String
    num' = (round (num * (10.0 ^ (toFloat p)))) |> toString
  in
    dropRight 2 num' ++ "." ++ String.right 2 num'

-- Definitions

radius = 150

thickLine : LineStyle
thickLine = { defaultLine | width = 5, cap = Round }
