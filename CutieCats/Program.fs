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

type Star() =
    let size = Rand.instance.NextSingle() * 4f + 2f
    let mutable pos = Vector2(Rand.instance.NextSingle(), Rand.instance.NextSingle())

    member _.Update(elapsedSec) =
        let x = pos.X - 0.05f * elapsedSec
        pos <-
            if x < 0f
            then Vector2(x + 1f, Rand.instance.NextSingle())
            else Vector2(x, pos.Y)

    member _.Draw (viewport: Viewport) (spriteBatch: SpriteBatch) =
        spriteBatch.DrawPoint(viewport.GetScreenPos(pos), Color.Yellow, size)

type CutieCatsGame() as this =
    inherit Game()

    let graphics = new GraphicsDeviceManager(this)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    let mutable viewport = Viewport.Default
    let mutable stars = Array.init 100 (fun _ -> Star())

    override __.LoadContent() =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        viewport <- Viewport.Create(this.GraphicsDevice.Viewport.Bounds, Vector2.One, Vector2.One/2f, true)

    override __.Update(gameTime) =
        let ms = gameTime.ElapsedGameTime.TotalSeconds |> single
        stars |> Array.iter (fun s -> s.Update ms)

    override __.Draw(gameTime) =
        this.GraphicsDevice.Clear Color.Black
        spriteBatch.Begin()

        stars |> Array.iter (fun s -> s.Draw viewport spriteBatch)

        spriteBatch.End()

module Entry =
    [<EntryPoint>]
    let main _ =
        use game = new CutieCatsGame()
        game.Run()
        0
