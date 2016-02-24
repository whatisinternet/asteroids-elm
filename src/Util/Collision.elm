{-
   Special thanks to the following:
     https://developer.mozilla.org/en-US/docs/Games/Techniques/2D_collision_detection
     http://wscg.zcu.cz/wscg2004/Papers_2004_Full/B83.pdf

   This implements basic circle based collision detection using nbody removal to
   only inspect objects that are 'near' the base object. See the top link for the
   javascript implementation of the algorithm.

   As the MDN article explains a broad and narrow phase is optimal to ensure
   the code is only looking at close bodies. In this case that means bodies 100px
   in any direction.
-}


module Util.Collision (..) where


localBodies : { a | x : number, y : number, radius : number } -> List { b | radius : number', x : number', y : number' } -> List { b | radius : number', x : number', y : number' }
localBodies base objectList =
  let
    maxX =
      base.x + base.radius + 100

    maxY =
      base.y + base.radius + 100

    minX =
      base.x - base.radius - 100

    minY =
      base.y - base.radius - 100

    isInX obj =
      (obj.x <= maxX && obj.x >= minX) || ((obj.x - obj.radius) <= maxX && (obj.x + obj.radius) >= minX)

    isInY obj =
      (obj.y <= maxY && obj.y >= minY) || ((obj.y - obj.radius) <= maxY && (obj.y + obj.radius) >= minY)
  in
    objectList
      |> List.filter (\obj -> (isInX obj) == (isInY obj))


isColliding : { a | radius : Float, x : Float, y : Float } -> List { b | radius : Float, x : Float, y : Float } -> Bool
isColliding mainObject objectList =
  let
    dx testBody =
      mainObject.x - testBody.x

    dy testBody =
      mainObject.y - testBody.y

    distance dx' dy' =
      sqrt (dx' * dx' + dy' * dy')

    collide testBody distance' =
      distance' < mainObject.radius + testBody.radius
  in
    objectList
      |> List.any
          (\obj ->
            collide obj <| (distance <| dx obj) <| dy obj
          )


hasCollisions : { a | radius : Float, x : Float, y : Float } -> List { b | radius : Float, x : Float, y : Float } -> Bool
hasCollisions mainObject objectList =
  objectList
    |> localBodies mainObject
    |> isColliding mainObject
