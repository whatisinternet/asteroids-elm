module Game (..) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import Window
import Random
import Keyboard
import Debug
import Now
import Ship
import Asteroid


-- MODEL


type alias Game =
  { ship : Ship.Ship
  , asteroids : List Asteroid.Asteroid
  , initialSeed : Int
  , width : Int
  , height : Int
  }


initGame : Game
initGame =
  { ship = Ship.initShip
  , asteroids = [ Asteroid.initAsteroid (round Now.loadTime) 0 0 ]
  , initialSeed = (round Now.loadTime)
  , width = 9000
  , height = 9000
  }



-- UPDATE


type Action
  = NoOp
  | UpdateShip Ship.Keys
  | UpdateAsteroids
  | AddAsteroids
  | AddAsteroid Asteroid.Asteroid
  | UpdateWidthAndHeight ( Int, Int )
  | UpdateSeed


update : Action -> Game -> Game
update action game =
  case action of
    NoOp ->
      game

    UpdateShip keys ->
      { game | ship = Ship.update (Ship.UpdateShip keys) game.ship, initialSeed = (round Now.loadTime) }

    UpdateAsteroids ->
      let
        isOffCanvas a g =
          not (a.y - a.radius > (toFloat g.height)) &&
          not (a.x - a.radius > (toFloat g.width)) &&
          not (a.y - a.radius < (toFloat (-1 * g.height))) &&
          not (a.x - a.radius < (toFloat (-1 * g.width)))

      in
        { game | asteroids =
          List.indexedMap (\b a->
            if isOffCanvas a game then
              Asteroid.update (Asteroid.UpdateAsteroid) a
            else
              Asteroid.initAsteroid (game.initialSeed + (round Now.loadTime)  + b) game.width game.height
          ) game.asteroids}

    AddAsteroid asteroid ->
      { game | asteroids = asteroid :: game.asteroids }

    AddAsteroids ->
      if (List.length game.asteroids) < 20 then
        { game
          | asteroids =
              [0..(20 - (List.length game.asteroids))]
                |> List.map
                    (\a ->
                      Asteroid.initAsteroid
                        (game.initialSeed
                          + a
                        )
                        game.width game.height
                    )
        }
      else
        game


    UpdateWidthAndHeight ( width', height' ) ->
      { game | width = width', height = height' }

    UpdateSeed ->
      { game | initialSeed = game.initialSeed + (round Now.loadTime) + 1 }
        |> update AddAsteroids
        |> update UpdateAsteroids



-- VIEW


view : Game -> Element
view game =
  let
    ( w, h ) =
      ( toFloat game.width, toFloat game.height )

    _ =
      Debug.watch "GAME"
  in
    collage
      (round w)
      (round h)
      [ rect w h
          |> filled (rgb 0 0 0 )
      , Ship.view game.ship
      , toForm (asteroidsView game)
      ]


asteroidsView : Game -> Element
asteroidsView game =
  let
    ( w, h ) =
      ( toFloat game.width, toFloat game.height )

    asteroidPosition a =
      ( .x a, .y a )
        |> Debug.watch "Asteroid position"
  in
    collage
      (round w)
      (round h)
      (List.map
        (\a ->
          ngon 5 a.radius
            |> filled (rgb 47 56 61)
            |> move (asteroidPosition a)
            |> rotate (a.x * 0.02)
        )
        (game.asteroids)
      )



-- SIGNALS


size : Signal Action
size =
  Signal.sampleOn
    (Time.fps 60)
    Window.dimensions
      |> Signal.map UpdateWidthAndHeight


updateShipPosition : Signal Action
updateShipPosition =
  Signal.sampleOn
    (Time.fps 60)
    (Keyboard.arrows
      |> Signal.map UpdateShip
    )


updateSeed : Signal Action
updateSeed =
  Time.fps 60
    |> Signal.map (\_ -> UpdateSeed)

input : Signal Action
input =
  Signal.mergeMany
    [ size
    , updateSeed
    , updateShipPosition
    ]



game : Signal Game
game =
  input
    |> Signal.foldp update initGame


main : Signal Element
main =
  game
    |> Signal.map view
