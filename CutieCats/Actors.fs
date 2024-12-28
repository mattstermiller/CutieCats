namespace CutieCats

open Acadian.FSharp
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Audio
open Microsoft.Xna.Framework.Graphics;
open MonoGame.Extended
open CutieCats

module GameWorld =
    let rect = RectangleF(Vector2.Zero, SizeF(16f/9f, 1f))

    let relativeToAbsPos (relPos: Vector2) = rect.Position + rect.Size.Scale(relPos).ToVector2()

type IActor =
    abstract member Update: elapsedSeconds: float32 -> IActor list
    abstract member Draw: Viewport -> SpriteBatch -> unit

type Textures = {
    CutieCatShip: Texture2D
    MeanieMouseShip: Texture2D
}

type Sounds = {
    CatLaser: SoundEffect
    CatShieldUp: SoundEffect
    CatShieldDown: SoundEffect
    CatShieldHit: SoundEffect
    CatShipHit: SoundEffect
    CatShipExplode: SoundEffect
    CatTaunt: SoundEffect
    MouseFire: SoundEffect
    MouseShipHit: SoundEffect
    MouseShipExplode: SoundEffect
    MouseTaunt: SoundEffect
}

type ActorCollection(actors: IActor array) =
    interface IActor with
        member _.Update elapsedSec =
            actors |> Seq.collect (fun a -> a.Update elapsedSec) |> Seq.toList

        member _.Draw viewport spriteBatch =
            actors |> Array.iter (fun a -> a.Draw viewport spriteBatch)

type Star() =
    let size = random.NextSingle() * 4f + 2f
    let mutable pos = Vector2(random.NextSingle(), random.NextSingle()) |> GameWorld.relativeToAbsPos
    let speed = 0.05f

    interface IActor with
        member _.Update elapsedSec =
            let x = pos.X - (speed * elapsedSec)
            pos <-
                if x < 0f
                then Vector2(1f, random.NextSingle()) |> GameWorld.relativeToAbsPos
                else Vector2(x, pos.Y)
            []

        member _.Draw viewport spriteBatch =
            spriteBatch.DrawPoint(viewport.GetScreenPos(pos), Color.Yellow, size)

type StarSystem(count: int) =
    inherit ActorCollection(Array.init count (fun _ -> Star()))

type Projectile(initPos: Vector2, vel: Vector2, size: SizeF, color) =
    let mutable pos = initPos
    let collisionSize = size / 2f

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

type Weapon(getFirePos, refireTime, vel, size, color, sound: SoundEffect) =
    let mutable refireTimeLeft = 0f

    member val IsFiring = false with get, set

    member this.Update elapsedSec : IActor option =
        refireTimeLeft <- refireTimeLeft - elapsedSec |> max 0f
        if this.IsFiring && refireTimeLeft = 0f then
            refireTimeLeft <- refireTime
            sound.Play() |> ignore
            Some (Projectile(getFirePos (), vel, size, color))
        else
            None

type CatWeapon(getFirePos, sound) =
    inherit Weapon(getFirePos, 0.8f, Vector2(0.6f, 0f), SizeF(0.07f, 0.01f), Color.Cyan, sound)

type MouseWeapon(getFirePos, sound) as this =
    inherit Weapon(getFirePos, 0.5f, Vector2(-0.7f, 0f), SizeF(0.07f, 0.08f), Color.Red, sound)
    do this.IsFiring <- true

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
        let size = SizeF(maxWidth * this.Health, height).Scale viewport.ScreenSize
        spriteBatch.DrawRectangle(RectangleF(pos, size), color, min size.Height size.Width)

type IShip =
    abstract member CollisionRect: RectangleF
    abstract member Hit: damage: float32 -> bool

type CatShip(texture: Texture2D, sounds: Sounds) =
    let size = texture.SizeF.ScaleToWidth 0.17f
    let initPos = Vector2(0.2f, 0.5f) |> GameWorld.relativeToAbsPos
    let mutable pos = initPos
    let speed = 0.4f
    let mutable vel = Vector2.Zero
    let healthBar = HealthBar(false)

    let posBounds = GameWorld.rect |> RectangleF.inflatedBy (size * -1f)

    let getFirePos () = pos + Vector2(size.Width / 2f, 0f)

    member val Weapon = CatWeapon(getFirePos, sounds.CatLaser)

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

        member _.Hit damage =
            healthBar.Hit damage
            |>! fun destroyed ->
                if destroyed then sounds.CatShipExplode else sounds.CatShipHit
                |> fun sound -> sound.Play() |> ignore

type MouseShipController(posBounds: RectangleF) =
    let mutable dir = if random.NextDouble() > 0.5 then Up.Vec else Down.Vec
    let nextDesiredYDist () = random.NextSingle(0.05f, 0.25f)
    let mutable desiredYDist = nextDesiredYDist ()

    member _.GetDir (pos: Vector2) (catShipPos: Vector2) =
        let yDistToCatShip = catShipPos.Y - pos.Y
        // if pos has gone past desired distance from cat ship or has hit the world bounds, switch direction
        if dir = Down.Vec && (yDistToCatShip > desiredYDist || pos.Y <= posBounds.WorldBottom)
            || dir = Up.Vec && (yDistToCatShip < -desiredYDist || pos.Y >= posBounds.WorldTop)
        then
            dir <- dir * Vector2(0f, -1f)
            desiredYDist <- nextDesiredYDist ()
        dir

type MouseShip(texture: Texture2D, sounds: Sounds, catShip: CatShip) =
    let size = texture.SizeF.ScaleToWidth 0.2f
    let initPos = Vector2(0.85f, 0.5f) |> GameWorld.relativeToAbsPos
    let mutable pos = initPos
    let posBounds = GameWorld.rect |> RectangleF.inflatedBy (size * -1f)
    let speed = 0.35f
    let controller = MouseShipController(posBounds)
    let healthBar = HealthBar(true)

    let getFirePos () = pos + Vector2(-size.Width / 2f, 0f)
    let weapon = MouseWeapon(getFirePos, sounds.MouseFire)

    member _.Reset () =
        pos <- initPos
        healthBar.Reset ()

    interface IActor with
        member _.Update elapsedSec =
            let dir = controller.GetDir pos catShip.Pos
            let vel = dir * speed
            pos <- pos + (vel * elapsedSec) |> Vector2.clampIn posBounds
            weapon.Update elapsedSec |> Option.toList

        member _.Draw viewport spriteBatch =
            // TODO: tint when hit
            spriteBatch.Draw(texture, viewport.GetScreenRect(pos, size), Color.White)
            healthBar.Draw viewport spriteBatch

    interface IShip with
        member _.CollisionRect = RectangleF.ofPosSize(pos, size)

        member _.Hit damage =
            healthBar.Hit damage
            |>! fun destroyed ->
                if destroyed then sounds.MouseShipExplode else sounds.MouseShipHit
                |> fun sound -> sound.Play() |> ignore

type GameState(textures: Textures, sounds: Sounds, exitFunc) =
    let gameResetTime = 4f

    let stars = StarSystem(200)
    let catShip = CatShip(textures.CutieCatShip, sounds)
    let mouseShip = MouseShip(textures.MeanieMouseShip, sounds, catShip)
    let mutable gameResetTimer = None

    let mutable actors: IActor list = []

    let init () =
        actors <- [
            stars
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
            |> Seq.ofType<Projectile>
            |> Seq.iter (fun proj ->
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
