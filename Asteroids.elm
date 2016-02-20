module Asteroids (..) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Random
import Debug
import Now

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
      (x, seed1) = random -900 900 seed0
      (y, seed2) = random -900 900 seed1
      (vx, seed3) = random -1 1 seed2
      (vy, seed4) = random -1 1 seed3
      (radius, seed5) = random 75 90 seed4
      _ = Debug.watch "GameSeed" vx

  in
    { x           = x
    , y           = y
    , vx          = vx
    , vy          = vy
    , radius      = radius
    , direction   = Right
    }
    |> Debug.log "Asteroid"

asteroid : Asteroid
asteroid =
    { x           = 0
    , y           = 0
    , vx          = 0
    , vy          = 0
    , radius      = 0
    , direction   = Right
    }

game : Model
game =
  { ship = ship
  , asteroids = []
  , state = True
  , initialSeed = (round Now.loadTime)
  }

-- Update

update : Keys -> Model -> Model
update keys game =
  game
    |> updateSeed
    |> addAsteroids game.initialSeed
    |> asteroidPhysics 3.1
    |> updateShip keys
    |> shipPhysics 9.0


addAsteroids : Int -> Model -> Model
addAsteroids seed game =
  let
      _ = Debug.watch "Game" game.initialSeed
      oldAsteroids = game.asteroids
      oldGame = game
      updatedGame = updateSeed game
  in
     if List.length oldAsteroids < 100 then
      { game | asteroids = List.append oldAsteroids
          [buildAsteroid (.initialSeed game)]
        }
    else
      game


updateSeed : Model -> Model
updateSeed game =
  { game | initialSeed = game.initialSeed + round Now.loadTime}


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
  in
     { game | asteroids =
       oldAsteroids
        |> List.map (\a ->
          { asteroid |
            x = a.x + dt * a.vx,
            y = a.y + dt * a.vy,
            vx = a.vx,
            vy = a.vy,
            radius = a.radius,
            direction = a.direction
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

      astriodPosition a =
        (.x a, .y a)
          |> Debug.watch "Asteroid position"

  in
    collage w' h'
    (List.map (\a ->
      ngon 6 (a.radius)
        |> filled (rgb 255 0 255)
        |> move (astriodPosition a)
      ) (game.asteroids))



-- Signals

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update game input)


input : Signal (Keys)
input =
  let
      delta = Signal.map (\t -> t / 20) (fps 60)
  in
     Signal.sampleOn delta (Signal.map (\a -> a) Keyboard.arrows)
