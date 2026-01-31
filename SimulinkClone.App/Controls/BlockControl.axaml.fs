namespace SimulinkClone.App.Controls

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Markup.Xaml
open Avalonia.Media

type InputPortId =
    | In1
    | In2

type BlockControl() as this =
    inherit UserControl()

    let outputPortClicked = Event<BlockControl>()
    let inputPort1Clicked = Event<BlockControl>()
    let inputPort2Clicked = Event<BlockControl>()

    let setBorderHighlight (b: Border) (onOff: bool) (color: IBrush) =
        if onOff then
            b.BorderThickness <- Thickness(2.0)
            b.BorderBrush <- color
        else
            b.BorderThickness <- Thickness(1.0)
            b.BorderBrush <- SolidColorBrush(Color.Parse("#9aa0a6"))

    let hookHandledClick (c: Control) (fire: unit -> unit) =
        c.PointerPressed.Add(fun e ->
            e.Handled <- true
            fire()
        )
        c.PointerReleased.Add(fun e ->
            e.Handled <- true
            ()
        )

    do
        AvaloniaXamlLoader.Load(this)

        let outP = this.FindControl<Border>("OutputPort")
        let in1 = this.FindControl<Border>("InputPort1")
        let in2 = this.FindControl<Border>("InputPort2")

        hookHandledClick (outP :> Control) (fun () -> outputPortClicked.Trigger(this))
        hookHandledClick (in1 :> Control) (fun () -> inputPort1Clicked.Trigger(this))
        hookHandledClick (in2 :> Control) (fun () -> inputPort2Clicked.Trigger(this))

    // --- API used by MainWindow ---
    member _.OutputPortClicked = outputPortClicked.Publish
    member _.InputPort1Clicked = inputPort1Clicked.Publish
    member _.InputPort2Clicked = inputPort2Clicked.Publish

    member _.SetTitle(title: string) =
        let t = this.FindControl<TextBlock>("TitleText")
        t.Text <- title

    member _.SetOutputHighlight(onOff: bool) =
        let b = this.FindControl<Border>("OutputPort")
        setBorderHighlight b onOff Brushes.Gold

    member _.SetInputHighlight(port: InputPortId, onOff: bool) =
        match port with
        | In1 ->
            let b = this.FindControl<Border>("InputPort1")
            setBorderHighlight b onOff Brushes.OrangeRed
        | In2 ->
            let b = this.FindControl<Border>("InputPort2")
            setBorderHighlight b onOff Brushes.OrangeRed
