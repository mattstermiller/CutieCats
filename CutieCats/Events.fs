namespace CutieCats

open Microsoft.Xna.Framework

type Dir =
    | Up
    | Down
    | Right
    | Left
with
    member this.Vec =
        match this with
        | Up -> Vector2(0f, 1f)
        | Down -> Vector2(0f, -1f)
        | Right -> Vector2(1f, 0f)
        | Left -> Vector2(-1f, 0f)

type GameEvent =
    | CatShipDir of Vector2
    | CatShipFiring of bool
    | Exit
