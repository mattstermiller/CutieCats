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

    member _.IsFacingEnemy = vel.X > 0f
    member _.Rect = RectangleF.ofPosSize(pos, size)
    member _.Damage = 0.12f

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
    let initPos = Vector2(0.2f, 0.5f)
    let mutable pos = initPos
    let speed = 0.25f
    let mutable dir = Vector2.Zero

    let radius = size/2f
    let posMin = radius.ToVector2()
    let posMax = Vector2.One - posMin

    let getFirePos () = pos + Vector2(size.Width / 2f, 0f)

    member _.Reset () =
        pos <- initPos
        dir <- Vector2.Zero

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
    let initPos = Vector2(0.85f, 0.5f)
    let mutable pos = initPos
    let speed = 0.1f
    let mutable health = 1f

    let radius = size/2f
    let posMin = radius.ToVector2()
    let posMax = Vector2.One - posMin

    member _.Reset () =
        pos <- initPos
        health <- 1f

    member _.Rect = RectangleF.ofPosSize(pos, size)

    member _.Hit damage =
        health <- health - damage
        health < 0f

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
            // health bar
            let margin = 0.02f
            let maxWidth = 0.2f
            let height = 0.03f
            let barPos = viewport.GetScreenPos(Vector2(1.0f - margin - maxWidth, 1.0f - margin))
            let barSize = viewport.GetScreenSize(Vector2(maxWidth * health, height))
            spriteBatch.DrawRectangle(RectangleF(barPos, barSize), Color.Red, barSize.Height)

type GameState(exitFunc) =
    let gameResetTime = 4f

    let stars = Array.init 100 (fun _ -> Star())
    let catShip = CatShip()
    let mouseShip = MouseShip(catShip)
    let mutable gameResetTimer = None

    let mutable actors = []

    let init () =
        actors <- [
            yield! stars |> Seq.cast<IActor>
            catShip
            mouseShip
        ]
        catShip.Reset()
        mouseShip.Reset()
        gameResetTimer <- None

    do
        init ()

    member _.HandleInput (input: InputEvent) =
        match input with
        | CatShipDir dir ->
            catShip.SetDir dir
        | CatShipFiring isFiring ->
            catShip.Weapon.IsFiring <- isFiring
        | Exit ->
            exitFunc()

    member _.Update elapsedSec =
        // update actors and add new ones
        let newActors = actors |> List.collect (fun actor -> actor.Update elapsedSec)
        actors <- actors @ newActors

        match gameResetTimer with
        | Some time ->
            // game over
            let remaining = time - elapsedSec
            if remaining > 0f then
                gameResetTimer <- Some remaining
            else
                init ()
        | None ->
            // collisions
            let destroyed = ResizeArray<IActor>()
            actors
            |> List.choose (function :? Projectile as p -> Some p | _ -> None)
            |> List.iter (fun proj ->
                if proj.IsFacingEnemy && mouseShip.Rect.Intersects(proj.Rect) then
                    destroyed.Add proj
                    if mouseShip.Hit proj.Damage then
                        destroyed.Add mouseShip
                        gameResetTimer <- Some gameResetTime
            )
            actors <- actors |> List.except destroyed

    member _.Draw viewport spriteBatch =
        actors |> List.iter (fun a -> a.Draw viewport spriteBatch)
