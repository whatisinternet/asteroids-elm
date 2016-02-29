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
  , started : Bool
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
  , started = False
  }



-- UPDATE


type Action
  = NoOp
  | StartGame
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

    StartGame ->
      { game | started = True }

    ResetGame ->
      case game.ship.alive of
        False ->
          initGame

        _ ->
          game

    UpdateShip keys ->
      let
        shipIsAlive =
          case game.started of
            True ->
              game.asteroids
                |> Collision.hasCollisions game.ship
                |> not

            False ->
              True
      in
        case game.ship.alive && game.started of
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
        case game.started of
          True ->
            { game | shots = updateShots }

          False ->
            game

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
            case game.ship.alive && game.started of
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
        case game.started of
          True ->
            { game | shots = (shot (vxy ( x, y ))) :: game.shots }

          False ->
            game



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
      , toForm (gameStartView game)
      ]


elmLogo : Game -> Element
elmLogo game =
  let
    ( w, h ) =
      ( toFloat game.width, toFloat game.height )

    gameAlpha =
      case game.started of
        True ->
          0

        _ ->
          1
  in
    collage
      (round w)
      (round h)
      [ polygon [ ( 161.649, 152.782 ), ( 231.514, 82.916 ), ( 91.783, 82.916 ) ]
          |> filled (rgb 94 68 0)
          |> alpha gameAlpha
      , polygon [ ( 8.867, 0 ), ( 79.241, 70.375 ), ( 232.213, 70.375 ), ( 161.838, 0 ) ]
          |> filled (rgb 50 82 23)
          |> alpha gameAlpha
      , polygon [ ( 323.298, 143.724 ), ( 323.298, 0 ), ( 179.573, 0 ) ]
          |> filled (rgb 38 71 80)
          |> alpha gameAlpha
      , polygon [ ( 152.781, 161.649 ), ( 0, 8.868 ), ( 0, 314.432 ) ]
          |> filled (rgb 39 35 47)
          |> alpha gameAlpha
      , polygon [ ( 255.522, 246.655 ), ( 323.298, 314.432 ), ( 323.298, 178.879 ) ]
          |> filled (rgb 94 68 0)
          |> alpha gameAlpha
      , polygon [ ( 161.649, 170.517 ), ( 8.869, 323.298 ), ( 314.43, 323.298 ) ]
          |> filled (rgb 38 71 80)
          |> alpha gameAlpha
      , square 105
          |> filled (rgb 50 82 23)
          |> move ( 245, 165 )
          |> rotate (degrees 45)
          |> alpha gameAlpha
      ]


mouseView : Game -> Element
mouseView game =
  let
    ( w, h ) =
      ( toFloat game.width, toFloat game.height )

    gameAlpha =
      case game.started of
        True ->
          0

        _ ->
          1
  in
    collage
      (round w)
      (round h)
      [ toForm (image 150 150 "./assets/img/Blank_White_Mouse.png")
          |> alpha gameAlpha
      ]


keyboardView : Game -> Element
keyboardView game =
  let
    ( w, h ) =
      ( toFloat game.width, toFloat game.height )

    gameAlpha =
      case game.started of
        True ->
          0

        _ ->
          1
  in
    collage
      (round w)
      (round h)
      [ toForm (image 100 100 "./assets/img/Keyboard_White_W.png")
          |> alpha gameAlpha
      , toForm (image 100 100 "./assets/img/Keyboard_White_A.png")
          |> move ( -75.0, -75 )
          |> alpha gameAlpha
      , toForm (image 100 100 "./assets/img/Keyboard_White_S.png")
          |> move ( 0, -75 )
          |> alpha gameAlpha
      , toForm (image 100 100 "./assets/img/Keyboard_White_D.png")
          |> move ( 75, -75 )
          |> alpha gameAlpha
      ]


gameStartView : Game -> Element
gameStartView game =
  let
    ( w, h ) =
      ( toFloat game.width, toFloat game.height )

    titleTextStyle =
      { typeface = []
      , height = Just 80
      , color = (rgba 255 255 255 0.8)
      , bold = True
      , italic = False
      , line = Nothing
      }

    startTextStyle =
      { typeface = []
      , height = Just 20
      , color = (rgba 255 255 255 1.0)
      , bold = True
      , italic = False
      , line = Nothing
      }

    gameAlpha =
      case game.started of
        True ->
          0

        _ ->
          1
  in
    collage
      (round w)
      (round h)
      [ rect w h
          |> filled (rgba 55 23 82 0.6)
          |> alpha gameAlpha
      , toForm (elmLogo game)
          |> rotate (degrees 30)
          |> scale 2.0
          |> moveX (toFloat (game.width // -2))
          |> moveY (toFloat (game.height // -3) - 60)
      , toForm
          (container
            (round w)
            (round h)
            midLeft
            (leftAligned
              (Text.style
                titleTextStyle
                (Text.fromString ("Asteroids-elm"))
              )
            )
            |> opacity gameAlpha
          )
      , toForm
          (container
            (round w)
            (round h)
            bottomRight
            (rightAligned
              (Text.style
                startTextStyle
                (Text.fromString ("Click to start"))
              )
            )
            |> opacity gameAlpha
          )
      , toForm (keyboardView game)
          |> move ( w / 3, h / -4 )
      , toForm (mouseView game)
          |> move ( w / 3, h / -4 )
          |> moveX 200
          |> moveY -50
      ]
      |> clickable (Signal.message gameMailBox.address StartGame)


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
      case (game.ship.alive && game.started) || not game.started of
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
    (Time.fps 5)
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
