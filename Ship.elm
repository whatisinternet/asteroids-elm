module Ship (..) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Debug


-- MODEL


type alias Ship =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , radius : Float
  , firing : Bool
  , alive : Bool
  }


type alias Keys =
  { x : Int, y : Int }


initShip : Ship
initShip =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , radius = 10.0
  , firing = False
  , alive = True
  }



-- UPDATE


type Action
  = NoOp
  | UpdateShip Keys Bool


update : Action -> Ship -> Ship
update action ship =
  case action of
    NoOp ->
      ship

    UpdateShip keys alive ->
      let
        tempShip =
          { ship | vx = toFloat keys.x, vy = toFloat keys.y }
      in
        { ship
          | x = ship.x + 3.0 * tempShip.vx
          , y = ship.y + 3.0 * tempShip.vy
          , alive = alive
          , radius = ship.radius + 0.01
        }



-- VIEW


view : Ship -> Form
view ship =
  let
    position =
      ( .x ship, .y ship )
        |> Debug.watch "Ship Position"
  in
    ngon 3 ship.radius
      |> filled (rgb 167 167 167)
      |> move position
