[<AutoOpen>]
module Util

open Microsoft.Xna.Framework
open MonoGame.Extended
open System
open Microsoft.Xna.Framework.Graphics

type Vector2 with
    member this.ToPoint2 () = Point2(this.X, this.Y)
    static member normalizeOrZero (v: Vector2) = if v = Vector2.Zero then v else v |> Vector2.Normalize
    static member clampIn (rect: RectangleF) (v: Vector2) = Vector2.Clamp(v, rect.Position, rect.Position + rect.Size)
    static member ofPoint2 (p: Point2) = Vector2(p.X, p.Y)

type Size2 with
    member this.ToVector2 () = Vector2(this.Width, this.Height)
    member this.ToPoint () = Point(int this.Width, int this.Height)
    member this.Scale (scale: Vector2) = Size2(this.Width * scale.X, this.Height * scale.Y)
    member this.ScaleToWidth (width: float32) = Size2(width, this.Height * (width / this.Width))

type RectangleF with
    // Y axis is fipped in game world coords
    member this.WorldBottom = this.Top
    member this.WorldTop = this.Bottom
    static member inflatedBy (size: Size2) (rect: RectangleF) = RectangleF(rect.Position - size/2f, rect.Size + size)
    static member ofPosSize(pos: Vector2, size: Size2) = RectangleF(Point2(pos.X, pos.Y) - size/2f, size)

type Texture2D with
    member this.Size2 = Size2(float32 this.Width, float32 this.Height)

let random = Random()
