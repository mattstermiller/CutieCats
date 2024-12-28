[<AutoOpen>]
module Util

open System
open System.Collections
open FSharp.Reflection
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics
open MonoGame.Extended

module Seq =
    let ofType<'a> (s: IEnumerable) =
        seq {
            let e = s.GetEnumerator()
            while e.MoveNext() do
                match e.Current with
                | :? 'a as a -> yield a
                | _ -> ()
        }

type SizeF with
    member this.ToVector2 () = Vector2(this.Width, this.Height)
    member this.ToPoint () = Point(int this.Width, int this.Height)
    member this.Scale (scale: Vector2) = SizeF(this.Width * scale.X, this.Height * scale.Y)
    member this.ScaleToWidth (width: float32) = SizeF(width, this.Height * (width / this.Width))

type Vector2 with
    member this.ToPoint () = Point(int this.X, int this.Y)
    static member normalizeOrZero (v: Vector2) = if v = Vector2.Zero then v else v |> Vector2.Normalize
    static member clampIn (rect: RectangleF) (v: Vector2) = Vector2.Clamp(v, rect.Position, rect.Position + rect.Size.ToVector2())
    static member ofPoint (p: Point) = Vector2(float32 p.X, float32 p.Y)

type RectangleF with
    // Y axis is fipped in game world coords
    member this.WorldBottom = this.Top
    member this.WorldTop = this.Bottom
    static member inflatedBy (size: SizeF) (rect: RectangleF) = RectangleF(rect.Position - size/2f, rect.Size + size)
    static member ofPosSize(pos: Vector2, size: SizeF) = RectangleF(pos - size/2f, size)

type Rectangle with
    static member ofPosSize(pos: Vector2, size: SizeF) = Rectangle((pos - size/2f).ToPoint(), size.ToPoint())

type Texture2D with
    member this.SizeF = SizeF(float32 this.Width, float32 this.Height)

type ContentManager with
    member this.LoadRecordItems<'record> directory =
        let loadMethod = typeof<ContentManager>.GetMethod("Load")
        let path name =
            IO.Path.Combine(directory, name)
        FSharpType.GetRecordFields(typeof<'record>)
        |> Array.map (fun field ->
            loadMethod.MakeGenericMethod([|field.PropertyType|]).Invoke(this, [|path field.Name|])
        )
        |> fun args -> FSharpValue.MakeRecord(typeof<'record>, args) :?> 'record

let random = Random()
