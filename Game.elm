module Game (..) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (clickable)
import Time exposing (..)
import Window
import Random
import Keyboard
import Debug
import Now
import Ship
import Asteroid
import Text


-- MODEL


type alias Game =
  { ship : Ship.Ship
  , asteroids : List Asteroid.Asteroid
  , initialSeed : Int
  , width : Int
  , height : Int
  , score : Int
  }


initGame : Game
initGame =
  { ship = Ship.initShip
  , asteroids = [ ]
  , initialSeed = (round Now.loadTime)
  , width = 9000
  , height = 9000
  , score = 0
  }



-- UPDATE


type Action
  = NoOp
  | ResetGame
  | UpdateShip Ship.Keys
  | UpdateAsteroids
  | AddAsteroids
  | AddAsteroid Asteroid.Asteroid
  | UpdateWidthAndHeight ( Int, Int )
  | UpdateSeed
  | UpdateScore


update : Action -> Game -> Game
update action game =
  case action of
    NoOp ->
      game

    ResetGame ->
      case game.ship.alive of
        False ->
          initGame
        _ ->
          game

    UpdateShip keys ->
      let
        isKilling ship asteroid =
          not (ship.y - ship.radius <= asteroid.y - asteroid.radius)
            && not (ship.x - ship.radius <= asteroid.x - asteroid.radius)
            && not (ship.y + ship.radius >= asteroid.y + asteroid.radius)
            && not (ship.x + ship.radius >= asteroid.x + asteroid.radius)

        shipIsAlive =
          game.asteroids
            |> List.all
                (\asteroid ->
                  not (isKilling game.ship asteroid)
                )
            |> Debug.watch "ISALIVE"
      in
        case game.ship.alive of
          True ->
            { game
              | ship =
                  Ship.update
                    (Ship.UpdateShip keys shipIsAlive)
                    game.ship
              , initialSeed = (round Now.loadTime)
            }

          _ ->
            game

    UpdateAsteroids ->
      let
        isOffCanvas a g =
          not (a.y - a.radius > (toFloat g.height))
            && not (a.x - a.radius > (toFloat g.width))
            && not (a.y - a.radius < (toFloat (-1 * g.height)))
            && not (a.x - a.radius < (toFloat (-1 * g.width)))
      in
        { game
          | asteroids =
              List.indexedMap
                (\b a ->
                  if isOffCanvas a game then
                    Asteroid.update (Asteroid.UpdateAsteroid) a
                  else
                    Asteroid.initAsteroid (game.initialSeed + (round Now.loadTime) + b) game.width game.height
                )
                game.asteroids
        }

    AddAsteroid asteroid ->
      { game | asteroids = asteroid :: game.asteroids }

    AddAsteroids ->
      if (List.length game.asteroids) < 90 then
        { game
          | asteroids =
              [0..(90 - (List.length game.asteroids))]
                |> List.map
                    (\a ->
                      Asteroid.initAsteroid
                        (game.initialSeed
                          + a
                        )
                        game.width
                        game.height
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

    UpdateScore ->
      { game
        | score =
            case game.ship.alive of
              True ->
                game.score + 1

              False ->
                game.score
      }



-- VIEW


view : Game -> Element
view game =
  let
    ( w, h ) =
      ( toFloat game.width, toFloat game.height )

    scoreStyle =
      { typeface = []
      , height = Just 90
      , color = (rgba 140 30 30 0.5)
      , bold = True
      , italic = False
      , line = Nothing
      }

    _ =
      Debug.watch "GAME"
  in
    collage
      (round w)
      (round h)
      [ rect w h
          |> filled (rgb 0 0 0)
      , Ship.view game.ship
      , toForm (asteroidsView game)
      , toForm
          (container
            (round w)
            (round h)
            topLeft
            (leftAligned
              (Text.style
                scoreStyle
                (Text.fromString (toString game.score))
              )
            )
          )
      , toForm (gameOverView game)
      ]


gameOverView : Game -> Element
gameOverView game =
  let
    ( w, h ) =
      ( toFloat game.width, toFloat game.height )

    textStyle =
      { typeface = []
      , height = Just 200
      , color = (rgba 0 0 0 1.0)
      , bold = True
      , italic = False
      , line = Nothing
      }

    gameAlpha =
      case game.ship.alive of
        True ->
          0

        _ ->
          1
  in
    collage
      (round w)
      (round h)
      [ rect w h
          |> filled (rgba 200 30 30 0.6)
          |> alpha gameAlpha
      , toForm
          (container
            (round w)
            (round h)
            middle
            (leftAligned
              (Text.style
                textStyle
                (Text.fromString ("GAME \"OVER\" \n Score: " ++ toString game.score))
              )
            )
            |> opacity gameAlpha
          )
      ]
      |> clickable (Signal.message gameMailBox.address ResetGame)


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


gameMailBox : Signal.Mailbox Action
gameMailBox =
  Signal.mailbox NoOp


score : Signal Action
score =
  (Time.every Time.second)
    |> Signal.map (\_ -> UpdateScore)


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
    , gameMailBox.signal
    , updateSeed
    , updateShipPosition
    , score
    ]


game : Signal Game
game =
  input
    |> Signal.foldp update initGame


main : Signal Element
main =
  game
    |> Signal.map view
