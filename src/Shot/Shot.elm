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
  , radius = 2.0
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
      let
          tempShot =
            { shot
            | vx = shot.vx + (shot.vx * 0.3)
            , vy = shot.vy + (shot.vy * 0.3)
          }
      in
        { shot
          | x = shot.x + 0.003 * tempShot.vx
          , y = shot.y + 0.003 * tempShot.vy
          , vx = tempShot.vx
          , vy = tempShot.vy
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
      |> rotate (shot.y * 30)
