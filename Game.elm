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
  , asteroids = [ Asteroid.initAsteroid (round Now.loadTime) ]
  , initialSeed = (round Now.loadTime)
  , width = 0
  , height = 0
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
        isOffCanvas a =
          (a.x + a.radius > (1080))
        filteredAsteroids =
          List.filter (\a -> isOffCanvas a) game.asteroids
      in
        { game | asteroids = List.map (\a -> Asteroid.update (Asteroid.UpdateAsteroid) a) game.asteroids}

    AddAsteroid asteroid ->
      { game | asteroids = asteroid :: game.asteroids }

    AddAsteroids ->
      if (List.length game.asteroids) < 20 then
        { game
          | asteroids =
              [0..20]
                |> List.map
                    (\a ->
                      Asteroid.initAsteroid
                        (game.initialSeed
                          + a
                        )
                    )
        }
      else
        game

    UpdateWidthAndHeight ( width', height' ) ->
      { game | width = width', height = height' }

    UpdateSeed ->
      { game | initialSeed = (round Now.loadTime) }



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
          |> filled (rgb 255 255 255)
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
  Window.dimensions
    |> Signal.map UpdateWidthAndHeight


updateShipPosition : Signal Action
updateShipPosition =
  Signal.sampleOn
    (Time.fps 60)
    (Keyboard.arrows
      |> Signal.map UpdateShip
    )


updateAsteroids : Signal Action
updateAsteroids =
  Time.fps 60
    |> Signal.map (\_ -> UpdateAsteroids)

addAsteroids : Signal Action
addAsteroids =
  Time.fps 10
    |> Signal.map (\_ -> AddAsteroids)


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
    , addAsteroids
    , updateAsteroids
    ]


game : Signal Game
game =
  input
    |> Signal.foldp update initGame


main : Signal Element
main =
  game
    |> Signal.map view
