module Game (..) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input exposing (clickable)
import Time exposing (..)
import Util.Now as Now
import Ship.Ship as Ship
import Asteroid.Asteroid as Asteroid
import Shot.Shot as Shot
import Util.Collision as Collision
import Window
import Random
import Keyboard
import Mouse
import Text
import Debug


-- MODEL


type alias Game =
  { ship : Ship.Ship
  , asteroids : List Asteroid.Asteroid
  , shots : List Shot.Shot
  , initialSeed : Int
  , width : Int
  , height : Int
  , score : Int
  }


initGame : Game
initGame =
  { ship = Ship.initShip
  , asteroids = []
  , shots = []
  , initialSeed = (round Now.loadTime)
  , width = 800
  , height = 600
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
  | Fire ( Int, Int )
  | UpdateShots


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
        shipIsAlive =
          game.asteroids
            |> Collision.hasCollisions game.ship
            |> not
      in
        case game.ship.alive of
          True ->
            { game
              | ship =
                  Ship.update
                    (Ship.UpdateShip keys shipIsAlive)
                    game.ship
              , initialSeed = game.initialSeed + (round Now.loadTime + 56)
            }

          _ ->
            { game | shots = [] }

    UpdateAsteroids ->
      let
        isOffCanvas a g =
          not (a.y - a.radius > (toFloat g.height))
            && not (a.x - a.radius > (toFloat g.width))
            && not (a.y - a.radius < (toFloat (-1 * g.height)))
            && not (a.x - a.radius < (toFloat (-1 * g.width)))

        aliveAsteroids asteroid =
          game.shots
            |> Collision.hasCollisions asteroid
            |> not
      in
        { game
          | asteroids =
              List.indexedMap
                (\b a ->
                  if (isOffCanvas a game) && (aliveAsteroids a) then
                    Asteroid.update (Asteroid.UpdateAsteroid) a
                  else
                    Asteroid.initAsteroid (game.initialSeed + (round (Now.loadTime + Now.loadTime)) + b) game.width game.height
                )
                game.asteroids
        }

    UpdateShots ->
      let
        isOffCanvas a g =
          not (a.y - a.radius > (toFloat g.height))
            && not (a.x - a.radius > (toFloat g.width))
            && not (a.y - a.radius < (toFloat (-1 * g.height)))
            && not (a.x - a.radius < (toFloat (-1 * g.width)))

        updateShots =
          game.shots
            |> List.filter (\shot -> isOffCanvas shot game)
            |> List.map (\a -> Shot.update (Shot.UpdateShot) a)
      in
        { game | shots = updateShots }

    AddAsteroid asteroid ->
      { game | asteroids = asteroid :: game.asteroids }

    AddAsteroids ->
      if (List.length game.asteroids) < 25 then
        { game
          | asteroids =
              [0..(24 - (List.length game.asteroids))]
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
        |> update UpdateShots
        |> update UpdateAsteroids

    UpdateScore ->
      { game
        | score =
            case game.ship.alive of
              True ->
                game.score + 6

              False ->
                game.score
      }

    Fire ( x, y ) ->
      let
        shot =
          (Shot.initShot game.ship.x game.ship.y)

        vxy ( x', y' ) =
          (,) (x' - absPositionX) (absPositionY - y')

        absPositionX =
          (game.width // 2) + (round (game.ship.x))

        absPositionY =
          (game.height // 2) - (round (game.ship.y))
      in
        { game | shots = (shot (vxy ( x, y ))) :: game.shots }



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
  in
    collage
      (round w)
      (round h)
      [ toForm (shotsView game)
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
                (Text.fromString ("GAME OVER \n Score: " ++ toString game.score))
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
  in
    collage
      (round w)
      (round h)
      (List.map
        (\a ->
          Asteroid.view a
        )
        (game.asteroids)
      )


shotsView : Game -> Element
shotsView game =
  let
    ( w, h ) =
      ( toFloat game.width, toFloat game.height )

    shotPosition a =
      ( .x a, .y a )
  in
    collage
      (round w)
      (round h)
      (List.map
        (\a ->
          Shot.view a
        )
        (game.shots)
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


fire : Signal Action
fire =
  Signal.sampleOn
    (Mouse.isDown)
    (Mouse.position
      |> Signal.map Fire
    )


updateShipPosition : Signal Action
updateShipPosition =
  Signal.sampleOn
    (Time.fps 60)
    (Keyboard.wasd
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
    , fire
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
