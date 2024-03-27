namespace CutieCats

open System.Reactive.Linq
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input;

type BindingSignals() =
    member val UpdateStart = Event<unit>()
    member val CatShipUp = Signal(false)
    member val CatShipDown = Signal(false)
    member val CatShipRight = Signal(false)
    member val CatShipLeft = Signal(false)
    member val CatShipFire = Signal(false)
    member val Exit = Event<unit>()
with
    member this.inputEvents () =
        [
            this.UpdateStart.Publish |> Observable.map (fun () ->
                seq {
                    if this.CatShipUp.Value then Up.Vec
                    if this.CatShipDown.Value then Down.Vec
                    if this.CatShipRight.Value then Right.Vec
                    if this.CatShipLeft.Value then Left.Vec
                }
                |> Seq.fold (+) Vector2.Zero
                |> CatShipDir
            )
            this.CatShipFire |> Observable.map CatShipFiring
            this.Exit.Publish |> Observable.map (fun () -> Exit)
        ] |> Observable.Merge

module KeyBinding =
    let bindings (signals: BindingSignals) = Map [
        Keys.Escape, ignore >> signals.Exit.Trigger
        Keys.Up, signals.CatShipUp.Trigger
        Keys.K, signals.CatShipUp.Trigger
        Keys.Down, signals.CatShipDown.Trigger
        Keys.J, signals.CatShipDown.Trigger
        Keys.Right, signals.CatShipRight.Trigger
        Keys.L, signals.CatShipRight.Trigger
        Keys.Left, signals.CatShipLeft.Trigger
        Keys.H, signals.CatShipLeft.Trigger
        Keys.Space, signals.CatShipFire.Trigger
    ]
