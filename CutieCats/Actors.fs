namespace CutieCats

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics;
open MonoGame.Extended
open CutieCats

module GameWorld =
    let rect = RectangleF(Point2.Zero, Size2(16f/9f, 1f))

    let relativeToAbsPos (relPos: Vector2) = rect.Position + (rect.Size.Scale relPos) |> Vector2.ofPoint2

type IActor =
    abstract member Update: elapsedSeconds: float32 -> IActor list
    abstract member Draw: Viewport -> SpriteBatch -> unit

type Textures = {
    CutieCatShip: Texture2D
    MeanieMouseShip: Texture2D
}

type Star() =
    let size = random.NextSingle() * 4f + 2f
    let mutable pos = Vector2(random.NextSingle(), random.NextSingle()) |> GameWorld.relativeToAbsPos
    let speed = 0.05f

    interface IActor with
        member _.Update(elapsedSec) =
            let x = pos.X - (speed * elapsedSec)
            pos <-
                if x < 0f
                then Vector2(1f, random.NextSingle()) |> GameWorld.relativeToAbsPos
                else Vector2(x, pos.Y)
            []

        member _.Draw viewport spriteBatch =
            spriteBatch.DrawPoint(viewport.GetScreenPos(pos), Color.Yellow, size)

type Projectile(initPos: Vector2, vel: Vector2, isEnemyWeapon) =
    let mutable pos = initPos
    let size = if isEnemyWeapon then Size2(0.04f, 0.04f) else Size2(0.06f, 0.01f)
    let collisionSize = size / 2f
    let color = if isEnemyWeapon then Color.Red else Color.Cyan

    let radius = size/2f

    member _.IsFacingEnemy = vel.X > 0f
    member _.CollisionRect = RectangleF.ofPosSize(pos, collisionSize)
    member _.Damage = 0.12f

    interface IActor with
        member _.Update elapsedSec =
            pos <- pos + vel * elapsedSec
            []

        member _.Draw viewport spriteBatch =
            let drawSize = viewport.GetScreenSize(radius)
            spriteBatch.DrawEllipse(viewport.GetScreenPos(pos), drawSize, 20, color, drawSize.Height)

type Weapon(getFirePos, vel, refireTime, isEnemyWeapon) =
    let mutable refireTimeLeft = 0f

    member val IsFiring = isEnemyWeapon with get, set

    member this.Update elapsedSec : IActor option =
        refireTimeLeft <- refireTimeLeft - elapsedSec |> max 0f
        if this.IsFiring && refireTimeLeft = 0f then
            refireTimeLeft <- refireTime
            Some (Projectile(getFirePos (), vel, isEnemyWeapon))
        else
            None

type HealthBar(isEnemy: bool) =
    let maxWidth = 0.2f
    let height = 0.02f
    let margin = 0.02f
    let posX = if isEnemy then 1f - margin - maxWidth else margin
    let pos = Vector2(posX, margin)
    let color = if isEnemy then Color.Red else Color.Green
    let mutable health = 1f

    member _.Health = health

    member _.Hit damage =
        health <- health - damage
        health < 0f

    member _.Reset () = health <- 1f

    member this.Draw (viewport: Viewport) (spriteBatch: SpriteBatch) =
        let pos = pos * viewport.ScreenSize
        let size = Size2(maxWidth * this.Health, height).Scale viewport.ScreenSize
        spriteBatch.DrawRectangle(RectangleF(pos, size), color, min size.Height size.Width)

type IShip =
    abstract member CollisionRect: RectangleF
    abstract member Hit: damage: float32 -> bool

type CatShip(texture: Texture2D) =
    let size = texture.Size2.ScaleToWidth 0.17f
    let initPos = Vector2(0.2f, 0.5f) |> GameWorld.relativeToAbsPos
    let mutable pos = initPos
    let speed = 0.35f
    let mutable vel = Vector2.Zero
    let healthBar = HealthBar(false)

    let posBounds = GameWorld.rect |> RectangleF.inflatedBy (size * -1f)

    let getFirePos () = pos + Vector2(size.Width / 2f, 0f)

    member val Weapon = Weapon(getFirePos, Vector2(0.3f, 0f), 0.5f, false)

    member _.Pos = pos

    member _.SetDir (dir: Vector2) =
        vel <- dir |> Vector2.normalizeOrZero |> (*) speed

    member _.Reset () =
        pos <- initPos
        vel <- Vector2.Zero
        healthBar.Reset ()

    interface IActor with
        member this.Update elapsedSec =
            pos <- pos + (vel * elapsedSec) |> Vector2.clampIn posBounds
            this.Weapon.Update elapsedSec |> Option.toList

        member _.Draw viewport spriteBatch =
            // TODO: tint when hit
            spriteBatch.Draw(texture, viewport.GetScreenRect(pos, size), Color.White)
            healthBar.Draw viewport spriteBatch

    interface IShip with
        member _.CollisionRect = RectangleF.ofPosSize(pos, size)
        member _.Hit damage = healthBar.Hit damage

type MouseShip(texture: Texture2D, catShip: CatShip) =
    let size = texture.Size2.ScaleToWidth 0.2f
    let initPos = Vector2(0.85f, 0.5f) |> GameWorld.relativeToAbsPos
    let mutable pos = initPos
    let speed = 0.15f
    let healthBar = HealthBar(true)

    let getFirePos () = pos + Vector2(-size.Width / 2f, 0f)
    let weapon = Weapon(getFirePos, Vector2(-0.3f, 0f), 1f, true)

    let posBounds = GameWorld.rect |> RectangleF.inflatedBy (size * -1f)

    member _.Reset () =
        pos <- initPos
        healthBar.Reset ()

    interface IActor with
        member _.Update elapsedSec =
            let dir = Vector2(0f, catShip.Pos.Y - pos.Y) |> Vector2.normalizeOrZero
            let vel = dir * speed
            pos <- pos + (vel * elapsedSec) |> Vector2.clampIn posBounds
            weapon.Update elapsedSec |> Option.toList

        member _.Draw viewport spriteBatch =
            // TODO: tint when hit
            spriteBatch.Draw(texture, viewport.GetScreenRect(pos, size), Color.White)
            healthBar.Draw viewport spriteBatch

    interface IShip with
        member _.CollisionRect = RectangleF.ofPosSize(pos, size)
        member _.Hit damage = healthBar.Hit damage

type GameState(textures: Textures, exitFunc) =
    let gameResetTime = 4f

    let stars = Array.init 100 (fun _ -> Star())
    let catShip = CatShip(textures.CutieCatShip)
    let mouseShip = MouseShip(textures.MeanieMouseShip, catShip)
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
                let ship: IShip = if proj.IsFacingEnemy then mouseShip else catShip
                if ship.CollisionRect.Intersects(proj.CollisionRect) then
                    destroyed.Add proj
                    if ship.Hit proj.Damage then
                        destroyed.Add (ship :?> IActor)
                        gameResetTimer <- Some gameResetTime
            )
            actors <- actors |> List.except destroyed

    member _.Draw viewport spriteBatch =
        actors |> List.iter (fun a -> a.Draw viewport spriteBatch)
