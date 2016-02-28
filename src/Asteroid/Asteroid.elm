module Asteroid.Asteroid (..) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Random
import Util.Now as Now


-- MODEL


type alias Asteroid =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , maxRadius : Float
  , radius : Float
  }


initAsteroid : Int -> Int -> Int -> Asteroid
initAsteroid startTime width height =
  let
    seed0 =
      Random.initialSeed (round (Now.loadTime) + startTime)

    random a b seed =
      (Random.generate (Random.float a b) seed)

    ( x, seed1 ) =
      random (toFloat (-1 * width)) (toFloat width) seed0

    ( y, seed2 ) =
      random (toFloat (-1 * height)) (toFloat height) seed1

    ( vx, seed3 ) =
      random -1 1 seed2

    ( vy, seed4 ) =
      random -1 1 seed3

    ( radius, seed5 ) =
      random 10 190 seed4

  in
    { x = x
    , y = y
    , vx = vx
    , vy = vy
    , maxRadius = radius
    , radius = 1
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
      let
          getRadius =
            case asteroid.radius < asteroid.maxRadius of
              True ->
                asteroid
                  |> .radius
                  |> (+) (0.7 * (abs asteroid.vx + asteroid.vy))
              _ ->
                asteroid
                  |> .radius

      in
        { asteroid
          | x = asteroid.x + 3.0 * asteroid.vx
          , y =
              asteroid.y
                + 3.0
                * asteroid.vy
          , radius = getRadius
        }



-- VIEW


view : Asteroid -> Form
view asteroid =
  let
    position =
      ( .x asteroid, .y asteroid )
  in
    ngon 5 asteroid.radius
      |> filled (rgb 47 56 61)
      |> move position
      |> rotate (asteroid.x * 0.02)
