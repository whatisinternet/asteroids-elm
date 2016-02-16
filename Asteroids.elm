module Asteroids (..) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Random
import Debug

-- Model


type alias Asteroid =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , radius : Float
  , direction : Direction
  }

type alias Ship =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , radius : Float
  , firing : Bool
  , alive : Bool
  }

type alias Model =
  { ship : Ship
  , asteroids : List Asteroid
  , state : Bool
  , initialSeed : Int
  }

type alias Keys = { x:Int, y:Int }

type Direction
  = Left
  | Right
  | Up
  | Down

ship : Ship
ship =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , radius = 10.0
  , firing = False
  , alive = True
  }

buildAsteroid : Int -> Asteroid
buildAsteroid startTime =
  let
      seed0 = Random.initialSeed startTime
      random a b seed = (Random.generate (Random.float a b) seed)
      (x, seed1) = random 0 90 seed0
      (y, seed2) = random 0 90 seed1
      (vx, seed3) = random -2 2 seed2
      (vy, seed4) = random -2 2 seed3
      (radius, seed5) = random 75 90 seed4
  in
    { x           = x
    , y           = y
    , vx          = vx
    , vy          = vy
    , radius      = radius
    , direction   = Right
    }

asteroid : Asteroid
asteroid =
  let
      seed0 = Random.initialSeed 12348192374081273481723478123847108234
      random a b seed = (Random.generate (Random.float a b) seed)
      (x, seed1) = random 0 90 seed0
      (y, seed2) = random 0 90 seed1
      (vx, seed3) = random -0.2 0.2 seed2
      (vy, seed4) = random -0.2 0.2 seed3
      (radius, seed5) = random 75 90 seed4
  in
    { x           = x
    , y           = y
    , vx          = vx
    , vy          = vy
    , radius      = radius
    , direction   = Right
    }

game : Model
game =
  { ship = ship
  , asteroids = []
  , state = True
  , initialSeed = 1234812834
  }

-- Update

update : Keys -> Model -> Model
update keys game =
  game
    |> updateSeed
    |> addAsteroids game.initialSeed
    |> asteroidPhysics 0.01
    |> updateShip keys
    |> shipPhysics 9.0


addAsteroids : Int -> Model -> Model
addAsteroids seed game =
  let
      oldAsteroids = game.asteroids
      oldGame = game
      updatedGame = updateSeed game
  in
     if List.length oldAsteroids < 10 then
      { game | asteroids = List.append oldAsteroids
          [buildAsteroid (.initialSeed game)]
        }
    else
      game


updateSeed : Model -> Model
updateSeed game =
  { game | initialSeed = game.initialSeed + 3215}


updateShip: Keys -> Model -> Model
updateShip keys game =
  let
      oldShip = game.ship
  in
     { game | ship = { oldShip |

      vx = toFloat keys.x,
      vy = toFloat keys.y
    }}

shipPhysics : Float -> Model -> Model
shipPhysics dt game =
  let
      oldShip = game.ship
  in
     { game | ship = { oldShip |

      x = oldShip.x + dt * oldShip.vx,
      y = oldShip.y + dt * oldShip.vy
    }}


asteroidPhysics : Float -> Model -> Model
asteroidPhysics dt game =
  let
      oldAsteroids = game.asteroids
      ship = game.ship
  in
     { game | asteroids =
       oldAsteroids
        |> List.map (\a ->
          { asteroid |
            x = (a.x + dt * a.vx) + (ship.x * 0.1),
            y = (a.y + dt * a.vy) + (ship.y * 0.1)
          }
      )}

-- View

view : (Int, Int) -> Model -> Element
view (w', h') game =
  let
      (w, h) = (toFloat w', toFloat h')

      position =
        (.x (game.ship), .y (game.ship))
          |> Debug.watch "Ship Position"

      astriodPosition asteroid =
        (.x asteroid, .y asteroid)
          |> Debug.watch "Asteroid position"

  in
     collage w' h'
     [ rect w h
        |> filled (rgb 255 255 255 )
     , ngon 3 game.ship.radius
        |> filled (rgb 74 167 32)
        |> move position
     -- , toForm (show game)
     , toForm (asteroidsView (w', h') game)
     ]

asteroidsView : (Int, Int) -> Model -> Element
asteroidsView (w', h') game =
  let
      (w, h) = (toFloat w', toFloat h')

      astriodPosition asteroid =
        (.x asteroid, .y asteroid)
          |> Debug.watch "Asteroid position"

  in
    collage w' h'
    (List.map (\asteroid ->
      ngon 6 (asteroid.radius)
        |> filled (rgb 255 0 255)
        |> move (astriodPosition asteroid)
      ) (game.asteroids))



-- Signals

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update game input)


input : Signal (Keys)
input =
  let
      delta = Signal.map (\t -> t / 20) (fps 30)
  in
     Signal.sampleOn delta (Signal.map (\a -> a) Keyboard.arrows)
