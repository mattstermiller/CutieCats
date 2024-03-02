namespace SuperPong

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics;
open Microsoft.Xna.Framework.Input;
open MonoGame.Extended

[<AutoOpen>]
module Extensions =
    type Vector2 with
        member this.ToPoint2 () = Point2(this.X, this.Y)

    type Size2 with
        member this.ToVector2 () = Vector2(this.Width, this.Height)
        member this.Scale (scale: Vector2) = Size2(this.Width * scale.X, this.Height * scale.Y)

type Viewport = {
    SizeFactor: Vector2
    PosFactor: Vector2
    PosTranslate: Vector2
}
with
    member this.GetScreenPos (pos: Vector2) = ((pos + this.PosTranslate) * this.PosFactor).Round 0
    member this.GetScreenSize (size: Size2) = size.Scale this.SizeFactor

    member this.GetScreenRect (center: Vector2, size: Size2) =
        let topLeft = center + Vector2(-size.Width/2.f, size.Height/2.f)
        RectangleF(this.GetScreenPos(topLeft).ToPoint2(), this.GetScreenSize(size))

    static member Default = {
        SizeFactor = Vector2()
        PosFactor = Vector2()
        PosTranslate = Vector2()
    }

    static member Create(screenArea: Rectangle, cameraSize: Vector2, cameraCenter: Vector2, invertY) =
        let screenSize = screenArea.Size.ToVector2()
        let screenPos = screenArea.Location.ToVector2()
        let yFactor = Vector2(1.f, if invertY then -1.f else 1.f)
        let sizeFactor = screenSize / cameraSize
        {   SizeFactor = sizeFactor
            PosFactor = sizeFactor * yFactor
            PosTranslate =
                (cameraSize / 2.f - cameraCenter) +
                (screenPos * cameraSize) / screenSize * yFactor +
                (if invertY then Vector2(0.f, -cameraSize.Y) else Vector2.Zero)
        }

module Rand =
    let instance = Random()

type IUpdate =
    abstract member Update: elapsedSeconds: float32 -> unit

type IDraw =
    abstract member Draw: Viewport -> SpriteBatch -> unit

type Star() =
    let size = Rand.instance.NextSingle() * 4f + 2f
    let mutable pos = Vector2(Rand.instance.NextSingle(), Rand.instance.NextSingle())
    let speed = 0.05f

    interface IUpdate with
        member _.Update(elapsedSec) =
            let x = pos.X - (speed * elapsedSec)
            pos <-
                if x < 0f
                then Vector2(x + 1f, Rand.instance.NextSingle())
                else Vector2(x, pos.Y)

    interface IDraw with
        member _.Draw viewport spriteBatch =
            spriteBatch.DrawPoint(viewport.GetScreenPos(pos), Color.Yellow, size)

type CatShip() =
    let size = Size2(0.08f, 0.08f)
    let mutable pos = Vector2(0.2f, 0.5f)
    let mutable vel = Vector2.Zero
    let speed = 0.15f

    let radius = size/2f
    let posMin = radius.ToVector2()
    let posMax = Vector2.One - posMin

    member _.SetDir (dir: Vector2) =
        vel <-
            if dir = Vector2.Zero
            then dir
            else Vector2.Normalize dir * speed

    interface IUpdate with
        member _.Update elapsedSec =
            pos <-
                pos + (vel * elapsedSec)
                |> fun v -> Vector2.Clamp(v, posMin, posMax)

    interface IDraw with
        member _.Draw viewport spriteBatch =
            spriteBatch.DrawEllipse(viewport.GetScreenPos(pos), viewport.GetScreenSize(radius), 20, Color.Orange, 20f)

type CutieCatsGame() as this =
    inherit Game()

    let graphics = new GraphicsDeviceManager(this)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    let mutable viewport = Viewport.Default
    let stars = Array.init 100 (fun _ -> Star())
    let catShip = CatShip()

    override __.LoadContent() =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        viewport <- Viewport.Create(this.GraphicsDevice.Viewport.Bounds, Vector2.One, Vector2.One/2f, true)

    override __.Update(gameTime) =
        let ms = gameTime.GetElapsedSeconds() |> single
        let update (a: IUpdate) = a.Update ms

        let pressedKeys = Keyboard.GetState().GetPressedKeys()
        let isPressed key = pressedKeys |> Array.contains key
        if isPressed Keys.Escape then
            this.Exit()

        catShip.SetDir (
            seq {
                if isPressed Keys.Up || isPressed Keys.K then Vector2(0f, 1f)
                if isPressed Keys.Down || isPressed Keys.J then Vector2(0f, -1f)
                if isPressed Keys.Right || isPressed Keys.L then Vector2(1f, 0f)
                if isPressed Keys.Left || isPressed Keys.H then Vector2(-1f, 0f)
            } |> Seq.fold (+) Vector2.Zero
        )

        stars |> Array.iter update
        update catShip

    override __.Draw(gameTime) =
        this.GraphicsDevice.Clear Color.Black
        let draw (a: IDraw) = a.Draw viewport spriteBatch

        spriteBatch.Begin()

        stars |> Array.iter draw
        draw catShip

        spriteBatch.End()

module Entry =
    [<EntryPoint>]
    let main _ =
        use game = new CutieCatsGame()
        game.Run()
        0
