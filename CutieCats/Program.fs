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
        static member NormalizeOrZero (v: Vector2) = if v = Vector2.Zero then v else v |> Vector2.Normalize

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
    let mutable dir = Vector2.Zero
    let speed = 0.25f

    let radius = size/2f
    let posMin = radius.ToVector2()
    let posMax = Vector2.One - posMin

    member _.Pos = pos

    interface IUpdate with
        member _.Update elapsedSec =
            let vel = (Vector2.NormalizeOrZero dir) * speed
            pos <-
                pos + (vel * elapsedSec)
                |> fun v -> Vector2.Clamp(v, posMin, posMax)

    member _.AddDir (dirChange: Vector2) =
        dir <- dir + dirChange

    interface IDraw with
        member _.Draw viewport spriteBatch =
            spriteBatch.DrawEllipse(viewport.GetScreenPos(pos), viewport.GetScreenSize(radius), 20, Color.Orange, 20f)

type MouseShip(catShip: CatShip) =
    let size = Size2(0.10f, 0.12f)
    let mutable pos = Vector2(0.8f, 0.5f)
    let speed = 0.1f

    let radius = size/2f
    let posMin = radius.ToVector2()
    let posMax = Vector2.One - posMin

    interface IUpdate with
        member _.Update elapsedSec =
            let dir = Vector2(0f, catShip.Pos.Y - pos.Y) |> Vector2.NormalizeOrZero
            let vel = dir * speed
            pos <-
                pos + (vel * elapsedSec)
                |> fun v -> Vector2.Clamp(v, posMin, posMax)

    interface IDraw with
        member _.Draw viewport spriteBatch =
            spriteBatch.DrawEllipse(viewport.GetScreenPos(pos), viewport.GetScreenSize(radius), 20, Color.Gray, 30f)

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

type SignalState =
    | CatShipDir of Dir

type SignalAction =
    | Exit

type Signal =
    | Action of SignalAction
    | State of SignalState

type SignalEvent =
    | DoAction of SignalAction
    | StateChange of SignalState * bool

type GameState(exitFunc) =
    let stars = Array.init 100 (fun _ -> Star())
    let catShip = CatShip()
    let mouseShip = MouseShip(catShip)

    member _.HandleEvent (evt: SignalEvent) =
        match evt with
        | StateChange (CatShipDir dir, started) -> catShip.AddDir (dir.Vec * if started then 1f else -1f)
        | DoAction Exit -> exitFunc()

    member _.Update elapsedSec =
        let update (a: IUpdate) = a.Update elapsedSec
        stars |> Array.iter update
        update catShip
        update mouseShip

    member _.Draw viewport spriteBatch =
        let draw (a: IDraw) = a.Draw viewport spriteBatch
        stars |> Array.iter draw
        draw catShip
        draw mouseShip

module KeyBinding =
    let bindings = Map [
        Keys.Escape, Action Exit
        Keys.Up, State (CatShipDir Up)
        Keys.K, State (CatShipDir Up)
        Keys.Down, State (CatShipDir Down)
        Keys.J, State (CatShipDir Down)
        Keys.Right, State (CatShipDir Right)
        Keys.L, State (CatShipDir Right)
        Keys.Left, State (CatShipDir Left)
        Keys.H, State (CatShipDir Left)
    ]

type CutieCatsGame() as this =
    inherit Game()

    let graphics = new GraphicsDeviceManager(this)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    let mutable viewport = Viewport.Default
    let state = GameState(this.Exit)
    let mutable signalState = []

    override __.LoadContent() =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        viewport <- Viewport.Create(this.GraphicsDevice.Viewport.Bounds, Vector2.One, Vector2.One/2f, true)

    override __.Update(gameTime) =
        let signals =
            Keyboard.GetState().GetPressedKeys()
            |> Seq.choose (fun key -> Map.tryFind key KeyBinding.bindings)
            |> Seq.toList
        let newSignalState = signals |> List.choose (function State s -> Some s | _ -> None)

        signals
        |> Seq.choose (function
            | Action a -> Some (DoAction a)
            | State s -> if not (signalState |> List.contains s) then Some (StateChange (s, true)) else None
        )
        |> Seq.append (
            signalState |> Seq.except newSignalState |> Seq.map (fun s -> StateChange (s, false))
        )
        |> Seq.iter state.HandleEvent

        signalState <- newSignalState

        state.Update (gameTime.GetElapsedSeconds() |> single)

    override __.Draw(gameTime) =
        this.GraphicsDevice.Clear Color.Black

        spriteBatch.Begin()

        state.Draw viewport spriteBatch

        spriteBatch.End()

module Entry =
    [<EntryPoint>]
    let main _ =
        use game = new CutieCatsGame()
        game.Run()
        0
