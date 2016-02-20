module Asteroid (..) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Random
import Debug
import Now


-- MODEL


type alias Asteroid =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , radius : Float
  }


initAsteroid : Int -> Asteroid
initAsteroid startTime =
  let
    seed0 =
      Random.initialSeed (round (Now.loadTime) + startTime)

    random a b seed =
      (Random.generate (Random.float a b) seed)

    ( x, seed1 ) =
      random -900 900 seed0

    ( y, seed2 ) =
      random -900 900 seed1

    ( vx, seed3 ) =
      random -1 1 seed2

    ( vy, seed4 ) =
      random -1 1 seed3

    ( radius, seed5 ) =
      random 20 190 seed4

    _ =
      Debug.watch "GameSeed" vx
  in
    { x = x
    , y = y
    , vx = vx
    , vy = vy
    , radius = radius
    }



-- UPDATE


type Action
  = NoOp
  | UpdateAsteroid


update : Action -> Asteroid -> Asteroid
update action asteroid =
  case action of
    NoOp ->
      asteroid

    UpdateAsteroid ->
      { asteroid
        | x = asteroid.x + 3.0 * asteroid.vx
        , y =
            asteroid.y
              + 3.0
              * asteroid.vy
      }



-- VIEW


view : Asteroid -> Form
view asteroid =
  let
    position =
      ( .x asteroid, .y asteroid )
        |> Debug.watch "Asteroid Position"
  in
    ngon 5 asteroid.radius
      |> filled (rgb 47 56 61)
      |> move position
      |> rotate (asteroid.x * 0.02)
