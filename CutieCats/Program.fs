namespace SuperPong

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics;
open Microsoft.Xna.Framework.Input;

type CutieCatsGame() as this = 
    inherit Game()

    let graphics = new GraphicsDeviceManager(this)

    override __.LoadContent() = 
        ()

    override __.Update(gameTime) =
        ()

    override __.Draw(gameTime) =
        this.GraphicsDevice.Clear Color.Black

module Entry =
    [<EntryPoint>]
    let main _ =
        use game = new CutieCatsGame()
        game.Run()
        0
