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
    member this.Scale (scale: Vector2) = Size2(this.Width * scale.X, this.Height * scale.Y)

type Signal<'a>(initialValue: 'a) =
    let evt = Event<'a>()
    let obs = evt.Publish
    let mutable value = initialValue

    interface IObservable<'a> with
        member _.Subscribe o = obs.Subscribe o

    member _.Value = value

    member _.Trigger newValue =
        value <- newValue
        evt.Trigger newValue

let random = Random()
