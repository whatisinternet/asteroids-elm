module Shot.Shot (..) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Debug


-- MODEL


type alias Shot =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , radius : Float
  }


initShot : Float -> Float -> (Int, Int) -> Shot
initShot x' y' (vx', vy') =
  { x = x'
  , y = y'
  , vx = toFloat vx'
  , vy = toFloat vy'
  , radius = 1.0
  }



-- UPDATE


type Action
  = NoOp
  | UpdateShot


update : Action -> Shot -> Shot
update action shot =
  case action of
    NoOp ->
      shot

    UpdateShot ->
      { shot
        | x = shot.x + 0.03 * shot.vx
        , y = shot.y + 0.03 * shot.vy
      }



-- VIEW


view : Shot -> Form
view shot =
  let
    position =
      ( .x shot, .y shot )
  in
    ngon 4 shot.radius
      |> filled (rgb 255 0 255)
      |> move position
