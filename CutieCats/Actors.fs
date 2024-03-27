namespace CutieCats

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics;
open MonoGame.Extended
open CutieCats

type UpdateResult = {
    GameEvents: GameEvent list
    IsDestroyed: bool
}
with
    static member None = { GameEvents = []; IsDestroyed = false }
    static member Event event = { GameEvents = [event]; IsDestroyed = false }
    static member Destroy = { GameEvents = []; IsDestroyed = true }
    static member DestroyWithEvent event = { GameEvents = [event]; IsDestroyed = true }

type IActor =
    abstract member Update: elapsedSeconds: float32 -> UpdateResult
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
            UpdateResult.None

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

    member _.GetWeaponPos () = pos + Vector2(size.Width / 2f, 0f)

    member _.SetDir (newDir: Vector2) = dir <- newDir

    interface IActor with
        member _.Update elapsedSec =
            let vel = (Vector2.NormalizeOrZero dir) * speed
            pos <-
                pos + (vel * elapsedSec)
                |> fun v -> Vector2.Clamp(v, posMin, posMax)
            UpdateResult.None

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
            UpdateResult.None

        member _.Draw viewport spriteBatch =
            spriteBatch.DrawEllipse(viewport.GetScreenPos(pos), viewport.GetScreenSize(radius), 20, Color.Gray, 30f)

type Projectile(initPos: Vector2, vel: Vector2) =
    let mutable pos = initPos
    let size = Size2(0.02f, 0.015f)

    let radius = size/2f

    interface IActor with
        member _.Update elapsedSec =
            pos <- pos + vel * elapsedSec
            // TODO: detect collision and create Hit event
            if pos.X > (1.0f + size.Width) then
                UpdateResult.Destroy
            else
                UpdateResult.None

        member _.Draw viewport spriteBatch =
            spriteBatch.DrawEllipse(viewport.GetScreenPos(pos), viewport.GetScreenSize(radius), 20, Color.Red, 5f)

type Weapon() =
    let refireTime = 0.5f
    let mutable refireTimeLeft = 0f

    member val IsFiring = false with get, set

    interface IActor with
        member this.Update elapsedSec =
            refireTimeLeft <- refireTimeLeft - elapsedSec |> max 0f
            if this.IsFiring && refireTimeLeft = 0f then
                refireTimeLeft <- refireTime
                UpdateResult.Event CatShipFire
            else
                UpdateResult.None

        member _.Draw viewport spriteBatch = ()

type GameState(exitFunc) =
    let stars = Array.init 100 (fun _ -> Star())
    let catShip = CatShip()
    let catShipWeapon = Weapon()
    let mouseShip = MouseShip(catShip)

    let mutable actors = [
        yield! stars |> Seq.cast<IActor>
        catShip
        mouseShip
        catShipWeapon
    ]

    let handleEvent (evt: GameEvent) : IActor list =
        [
            match evt with
            | CatShipFire ->
                let projectileVel = Vector2(0.2f, 0f)
                yield Projectile(catShip.GetWeaponPos(), projectileVel)
            | MouseShipHit pos ->
                () // TODO
        ]

    member _.HandleInput (input: InputEvent) =
        match input with
        | CatShipDir dir ->
            catShip.SetDir dir
        | CatShipFiring isFiring ->
            catShipWeapon.IsFiring <- isFiring
        | Exit ->
            exitFunc()

    member _.Update elapsedSec =
        actors <- actors |> List.collect (fun actor ->
            let result = actor.Update elapsedSec
            [
                if not result.IsDestroyed then
                    actor
                yield! result.GameEvents |> List.collect handleEvent
            ]
        )

        // TODO: separate Update into phases (it's weird that the outcome could depend on ordering of actors, like a projectile hit could affect a ship before its update)
        // - update (apply physics, fire weapon)
        //      can add actors
        // - collision detection and response
        //      can remove actors
        //      iterate over projectiles checking for collision with ships, shield, or no longer intersects world rect
        //      probably doesn't need to consider newly created actors

    member _.Draw viewport spriteBatch =
        actors |> List.iter (fun a -> a.Draw viewport spriteBatch)
