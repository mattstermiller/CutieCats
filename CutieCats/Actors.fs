namespace CutieCats

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics;
open MonoGame.Extended
open CutieCats

type IActor =
    abstract member Update: elapsedSeconds: float32 -> IActor list
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
            []

        member _.Draw viewport spriteBatch =
            spriteBatch.DrawPoint(viewport.GetScreenPos(pos), Color.Yellow, size)

type Projectile(initPos: Vector2, vel: Vector2) =
    let mutable pos = initPos
    let size = Size2(0.02f, 0.015f)

    let radius = size/2f

    interface IActor with
        member _.Update elapsedSec =
            pos <- pos + vel * elapsedSec
            []

        member _.Draw viewport spriteBatch =
            spriteBatch.DrawEllipse(viewport.GetScreenPos(pos), viewport.GetScreenSize(radius), 20, Color.Red, 5f)

type Weapon(getFirePos, vel) =
    let refireTime = 0.5f
    let mutable refireTimeLeft = 0f

    member val IsFiring = false with get, set

    member this.Update elapsedSec : IActor option =
        refireTimeLeft <- refireTimeLeft - elapsedSec |> max 0f
        if this.IsFiring && refireTimeLeft = 0f then
            refireTimeLeft <- refireTime
            Some (Projectile(getFirePos (), vel))
        else
            None

type CatShip() =
    let size = Size2(0.08f, 0.08f)
    let mutable pos = Vector2(0.2f, 0.5f)
    let mutable dir = Vector2.Zero
    let speed = 0.25f

    let radius = size/2f
    let posMin = radius.ToVector2()
    let posMax = Vector2.One - posMin

    let getFirePos () = pos + Vector2(size.Width / 2f, 0f)

    member val Weapon = Weapon(getFirePos, Vector2(0.2f, 0f))

    member _.Pos = pos

    member _.SetDir (newDir: Vector2) = dir <- newDir

    interface IActor with
        member this.Update elapsedSec =
            let vel = (Vector2.NormalizeOrZero dir) * speed
            pos <-
                pos + (vel * elapsedSec)
                |> fun v -> Vector2.Clamp(v, posMin, posMax)
            this.Weapon.Update elapsedSec |> Option.toList

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
            []

        member _.Draw viewport spriteBatch =
            spriteBatch.DrawEllipse(viewport.GetScreenPos(pos), viewport.GetScreenSize(radius), 20, Color.Gray, 30f)

type GameState(exitFunc) =
    let stars = Array.init 100 (fun _ -> Star())
    let catShip = CatShip()
    let mouseShip = MouseShip(catShip)

    let mutable actors = [
        yield! stars |> Seq.cast<IActor>
        catShip
        mouseShip
    ]

    member _.HandleInput (input: InputEvent) =
        match input with
        | CatShipDir dir ->
            catShip.SetDir dir
        | CatShipFiring isFiring ->
            catShip.Weapon.IsFiring <- isFiring
        | Exit ->
            exitFunc()

    member _.Update elapsedSec =
        let newActors = actors |> List.collect (fun actor -> actor.Update elapsedSec)

        // TODO: collision detection and response
        //      can remove actors
        //      probably doesn't need to consider newly created actors
        //      iterate over projectiles checking for collision with ships, shield, or no longer intersects world rect
        // let projectiles = actors |> Seq.choose (function :? Projectile as p -> Some p | _ -> None)

        actors <- actors @ newActors

    member _.Draw viewport spriteBatch =
        actors |> List.iter (fun a -> a.Draw viewport spriteBatch)
