open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics;
open Microsoft.Xna.Framework.Input;
open MonoGame.Extended
open CutieCats
open MonoGame.Extended.ViewportAdapters

type CutieCatsGame() as this =
    inherit Game()

    let graphics = new GraphicsDeviceManager(this)

    let mutable state = Unchecked.defaultof<GameState>
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let mutable viewport = Unchecked.defaultof<Viewport>
    let mutable camera = Unchecked.defaultof<OrthographicCamera>

    let keyEvents = Event<Keys * bool>()
    let signals = BindingSignals ()
    let mutable pressedKeys = [||]

    let generateKeyEvents () =
        let newPressedKeys = Keyboard.GetState().GetPressedKeys()
        Seq.append
            (pressedKeys |> Seq.except newPressedKeys |> Seq.map (fun k -> k, false))
            (newPressedKeys |> Seq.except pressedKeys |> Seq.map (fun k -> k, true))
        |> Seq.iter keyEvents.Trigger
        pressedKeys <- newPressedKeys

    override _.Initialize() =
        base.Initialize()

        graphics.PreferredBackBufferWidth <- 1280
        graphics.PreferredBackBufferHeight <- 720
        graphics.ApplyChanges()

        camera <-
            // TODO: rework positions and sizes based on this new virtual space instead of 1, 1
            new BoxingViewportAdapter(this.Window, this.GraphicsDevice, 1600, 900)
            |> OrthographicCamera


    override _.LoadContent() =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)

        // TODO: support resizing screen, fit viewport into screen, add clipping or letterboxing to fill area outside of viewport
        viewport <- Viewport(this.GraphicsDevice.Viewport.Bounds, GameWorld.rect.Size, GameWorld.rect.Center, true)

        let textures = this.Content.LoadRecordItems<Textures> "content/textures"
        let sounds = this.Content.LoadRecordItems<Sounds> "content/sounds"

        state <- GameState(textures, sounds, this.Exit)

        let bindMap = KeyBinding.bindings signals
        keyEvents.Publish.Add (fun (key, pressed) -> bindMap.TryFind key |> Option.iter (fun f -> f pressed))

        signals.inputEvents().Add(state.HandleInput)

    override _.Update(gameTime) =
        generateKeyEvents ()
        state.Update (gameTime.GetElapsedSeconds() |> single)

    override _.Draw(gameTime) =
        this.GraphicsDevice.Clear Color.Black

        spriteBatch.Begin(transformMatrix = camera.GetViewMatrix())
        state.Draw spriteBatch
        spriteBatch.End()

module Entry =
    [<EntryPoint>]
    let main _ =
        use game = new CutieCatsGame()
        game.Run()
        0
