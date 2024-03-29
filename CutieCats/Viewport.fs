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

    member _.GetScreenPos (pos: Vector2) = ((pos + posTranslate) * posFactor).Round 0
    member _.GetScreenSize (size: Size2) = size.Scale sizeFactor

    member this.GetScreenRect (center: Vector2, size: Size2) =
        let topLeft = center + Vector2(-size.Width/2.f, size.Height/2.f)
        Rectangle(this.GetScreenPos(topLeft).ToPoint(), this.GetScreenSize(size).ToPoint())

    // TODO: scale actor sizes to their texture size and game width using below functions

    // member this.GetCameraAspectRatio () =
    // member this.ScaleTextureSizeToGameWidth texture gameWidth =

    // Texture2D.GetAspectRatio texture =

    (* NOTE calculations:
        game: 1.0, 1.0
        screen: 800, 600

        actor image: 150, 100
        actor game width: 0.1

        actor screen width = 0.1 * 800 / 1.0 = 80
        actor screen height = 80 / 150 * 100 = 53.33
        actor game height = 53.33 / 600 * 1.0 = 0.0889

        formulate equation using the variables...

        actor game height =
            agw * sx / gx / aix * aiy / sy * gy
            (agw * sx * aiy * gy) / (gx * aix * sy)
            agw / (aix/aiy) * (sx/sy) / (gx/gy)

        viewport game aspect ratio =
            (sx/sy) / (gx/gy)
            (800/600) / (1.0/1.0)
            1.333

        actor game height = 0.1 / 1.5 * 1.333 = 0.0889
    *)
