module Shot.Shot (..) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Random
import Util.Now as Now


-- MODEL


type alias Shot =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , radius : Float
  , red : Int
  , green : Int
  , blue : Int
  }


initShot : Float -> Float -> ( Int, Int ) -> Shot
initShot x' y' ( vx', vy' ) =
  let
    seed0 =
      Random.initialSeed (round (Now.loadTime) + vx' + vy')

    random a b seed =
      (Random.generate (Random.int a b) seed)

    ( red, seed1 ) =
      random 5 255 seed0

    ( green, seed2 ) =
      random 5 255 seed1

    ( blue, seed3 ) =
      random 5 255 seed2
  in
    { x = x'
    , y = y'
    , vx = toFloat vx'
    , vy = toFloat vy'
    , radius = 7.0
    , red = red
    , green = green
    , blue = blue
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
          | x = shot.x + 3.0e-3 * tempShot.vx
          , y = shot.y + 3.0e-3 * tempShot.vy
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
      |> filled (rgb shot.red shot.green shot.blue)
      |> move position
      |> rotate (shot.y * 30)
