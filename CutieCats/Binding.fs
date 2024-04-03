namespace CutieCats

open System.Reactive.Linq
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input;

type BindingSignals() =
    member val CatShipUp = Event<bool>()
    member val CatShipDown = Event<bool>()
    member val CatShipRight = Event<bool>()
    member val CatShipLeft = Event<bool>()
    member val CatShipFire = Event<bool>()
    member val Exit = Event<unit>()
with
    member this.inputEvents () =
        [
            Observable.Merge [|
                this.CatShipUp.Publish |> Observable.map (fun v -> (Up, v))
                this.CatShipDown.Publish |> Observable.map (fun v -> (Down, v))
                this.CatShipRight.Publish |> Observable.map (fun v -> (Right, v))
                this.CatShipLeft.Publish |> Observable.map (fun v -> (Left, v))
            |]
            |> Observable.scan (fun dirSet (dir, toggle) -> (if toggle then Set.add else Set.remove) dir dirSet) Set.empty
            |> Observable.map (Seq.map (fun dir -> dir.Vec) >> Seq.fold (+) Vector2.Zero >> CatShipDir)

            this.CatShipFire.Publish |> Observable.map CatShipFiring
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
