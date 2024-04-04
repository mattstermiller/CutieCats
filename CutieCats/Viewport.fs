namespace CutieCats

open Microsoft.Xna.Framework
open MonoGame.Extended

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

    member _.ScreenSize = screenSize

    member _.GetScreenPos (gamePos: Vector2) = ((gamePos + posTranslate) * posFactor).Round 0
    member _.GetScreenSize (gameSize: Size2) = gameSize.Scale sizeFactor

    member this.GetScreenRect (center: Vector2, size: Size2) =
        let topLeft = center + Vector2(-size.Width/2f, size.Height/2f)
        Rectangle(this.GetScreenPos(topLeft).ToPoint(), this.GetScreenSize(size).ToPoint())
