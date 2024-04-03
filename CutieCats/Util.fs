[<AutoOpen>]
module Util

open Microsoft.Xna.Framework
open MonoGame.Extended
open System

type Vector2 with
    member this.ToPoint2 () = Point2(this.X, this.Y)
    static member NormalizeOrZero (v: Vector2) = if v = Vector2.Zero then v else v |> Vector2.Normalize

type Size2 with
    member this.ToVector2 () = Vector2(this.Width, this.Height)
    member this.ToPoint () = Point(int this.Width, int this.Height)
    member this.Scale (scale: Vector2) = Size2(this.Width * scale.X, this.Height * scale.Y)

type RectangleF with
    static member ofPosSize(pos: Vector2, size: Size2) =
        RectangleF(Point2(pos.X, pos.Y) - size/2f, size)

let random = Random()
