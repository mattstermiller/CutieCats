namespace CutieCats

open Microsoft.Xna.Framework
open MonoGame.Extended
open Microsoft.Xna.Framework.Graphics

type Viewport(screenArea: Rectangle, cameraSize: Vector2, cameraCenter: Vector2, invertY) =
    let screenSize = screenArea.Size.ToVector2()
    let screenPos = screenArea.Location.ToVector2()
    let yFactor = Vector2(1f, if invertY then -1f else 1f)

    let sizeFactor = screenSize / cameraSize
    let posFactor = sizeFactor * yFactor
    let posTranslate =
        (cameraSize / 2f - cameraCenter) +
        (screenPos * cameraSize) / screenSize * yFactor +
        (if invertY then Vector2(0f, -cameraSize.Y) else Vector2.Zero)

    let cameraAspectRatio = (screenSize.X / screenSize.Y) / (cameraSize.X / cameraSize.Y)

    member _.GetScreenPos (pos: Vector2) = ((pos + posTranslate) * posFactor).Round 0
    member _.GetScreenSize (size: Size2) = size.Scale sizeFactor

    member this.GetScreenRect (center: Vector2, size: Size2) =
        let topLeft = center + Vector2(-size.Width/2.f, size.Height/2.f)
        Rectangle(this.GetScreenPos(topLeft).ToPoint(), this.GetScreenSize(size).ToPoint())

    member _.ScaleTextureSizeToGameWidth (gameWidth: float32) (texture: Texture2D) =
        let gameHeight = gameWidth / texture.AspectRatio * cameraAspectRatio
        Size2(gameWidth, gameHeight)

    member _.ScaleVector (v: Vector2) = Vector2(v.X, v.Y * cameraAspectRatio)
