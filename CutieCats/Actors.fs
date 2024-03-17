namespace CutieCats

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics;
open MonoGame.Extended
open CutieCats

type IActor =
    abstract member Update: elapsedSeconds: float32 -> unit
    abstract member Draw: Viewport -> SpriteBatch -> unit

type Star() =
    let size = random.NextSingle() * 4f + 2f
    let mutable pos = Vector2(random.NextSingle(), random.NextSingle())
    let speed = 0.05f

    interface IActor with
        member _.Update(elapsedSec) =
            let x = pos.X - (speed * elapsedSec)
            pos <-
                if x < 0f
                then Vector2(x + 1f, random.NextSingle())
                else Vector2(x, pos.Y)

        member _.Draw viewport spriteBatch =
            spriteBatch.DrawPoint(viewport.GetScreenPos(pos), Color.Yellow, size)

type CatShip() =
    let size = Size2(0.08f, 0.08f)
    let mutable pos = Vector2(0.2f, 0.5f)
    let mutable dir = Vector2.Zero
    let speed = 0.25f

    let radius = size/2f
    let posMin = radius.ToVector2()
    let posMax = Vector2.One - posMin

    member _.Pos = pos

    member _.SetDir (newDir: Vector2) = dir <- newDir

    interface IActor with
        member _.Update elapsedSec =
            let vel = (Vector2.NormalizeOrZero dir) * speed
            pos <-
                pos + (vel * elapsedSec)
                |> fun v -> Vector2.Clamp(v, posMin, posMax)

        member _.Draw viewport spriteBatch =
            spriteBatch.DrawEllipse(viewport.GetScreenPos(pos), viewport.GetScreenSize(radius), 20, Color.Orange, 20f)

type MouseShip(catShip: CatShip) =
    let size = Size2(0.10f, 0.12f)
    let mutable pos = Vector2(0.8f, 0.5f)
    let speed = 0.1f

    let radius = size/2f
    let posMin = radius.ToVector2()
    let posMax = Vector2.One - posMin

    interface IActor with
        member _.Update elapsedSec =
            let dir = Vector2(0f, catShip.Pos.Y - pos.Y) |> Vector2.NormalizeOrZero
            let vel = dir * speed
            pos <-
                pos + (vel * elapsedSec)
                |> fun v -> Vector2.Clamp(v, posMin, posMax)

        member _.Draw viewport spriteBatch =
            spriteBatch.DrawEllipse(viewport.GetScreenPos(pos), viewport.GetScreenSize(radius), 20, Color.Gray, 30f)

type GameState(exitFunc) =
    let stars = Array.init 100 (fun _ -> Star())
    let catShip = CatShip()
    let mouseShip = MouseShip(catShip)

    let actors = [
        yield! stars |> Seq.cast<IActor>
        catShip
        mouseShip
    ]

    member _.HandleEvent (evt: GameEvent) =
        match evt with
        | CatShipDir dir -> catShip.SetDir dir
        | Exit -> exitFunc()

    member _.Update elapsedSec =
        actors |> List.iter (fun a -> a.Update elapsedSec)

    member _.Draw viewport spriteBatch =
        actors |> List.iter (fun a -> a.Draw viewport spriteBatch)
