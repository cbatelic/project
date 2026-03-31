namespace SimulinkClone.App.Controls

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Markup.Xaml
open Avalonia.Media
open Avalonia.Layout

type InputPortId =
    | In1
    | In2

type BlockControl() as this =
    inherit UserControl()

    // ---------------------------
    // Events
    // ---------------------------
    let outputPortClicked = Event<BlockControl>()
    let inputPort1Clicked = Event<BlockControl>()
    let inputPort2Clicked = Event<BlockControl>()

    // ---------------------------
    // Node state
    // ---------------------------
    let mutable nodeId = Guid.NewGuid().ToString("N")
    let mutable kind = "constant"
    let mutable constantValue: float option = Some 1.0
    let mutable integratorInitial: float option = Some 0.0

    // ---------------------------
    // Safe UI access
    // ---------------------------
    let tryFindTextBlock (name: string) =
        let c = this.FindControl<TextBlock>(name)
        if isNull c then None else Some c

    let tryFindBorder (name: string) =
        let c = this.FindControl<Border>(name)
        if isNull c then None else Some c

    let setTextIfExists (name: string) (value: string) =
        match tryFindTextBlock name with
        | Some tb -> tb.Text <- value
        | None -> ()

    let setBorderHighlight (b: Border) (onOff: bool) (color: IBrush) =
        if onOff then
            b.BorderThickness <- Thickness(2.0)
            b.BorderBrush <- color
        else
            b.BorderThickness <- Thickness(1.4)
            b.BorderBrush <- SolidColorBrush(Color.Parse("#91A4C3"))
            b.Background <- SolidColorBrush(Color.Parse("#1B2230"))

    let hookHandledClick (c: Control) (fire: unit -> unit) =
        c.PointerPressed.Add(fun e ->
            e.Handled <- true
            fire ()
        )
        c.PointerReleased.Add(fun e ->
            e.Handled <- true
            ()
        )

    let tryParseFloat (s: string) =
        let s2 = if isNull s then "" else s
        let ok, v =
            Double.TryParse(
                s2,
                Globalization.NumberStyles.Float,
                Globalization.CultureInfo.InvariantCulture
            )

        if ok then Some v else None

    let fmt (v: float) =
        v.ToString("0.###", Globalization.CultureInfo.InvariantCulture)

    let normalizeKind (k: string) =
        (if isNull k then "" else k).Trim().ToLowerInvariant()

    // ---------------------------
    // Icons
    // ---------------------------
    let iconForKind (k: string) =
        match normalizeKind k with
        | "constant" -> "C"
        | "add" -> "∑"
        | "subtract" -> "−"
        | "multiply" -> "×"
        | "integrator" -> "∫"
        | "gain" -> "k"
        | "constraint" -> "⎇"
        | _ -> "■"

    let titleForKind (k: string) =
        match normalizeKind k with
        | "constant" -> "Constant"
        | "add" -> "Add"
        | "subtract" -> "Subtract"
        | "multiply" -> "Multiply"
        | "integrator" -> "Integrator"
        | "gain" -> "Gain"
        | "constraint" -> "Constraint"
        | other when other.Length > 0 -> other
        | _ -> "Block"

    // ---------------------------
    // Refresh block text
    // ---------------------------
    let refreshText () =
        let k = normalizeKind kind

        setTextIfExists "TitleIcon" (iconForKind k)
        setTextIfExists "TitleText" (titleForKind k)

        let paramText =
            match k with
            | "constant" ->
                match constantValue with
                | Some v -> sprintf "C = %s" (fmt v)
                | None -> "C = ?"

            | "integrator" ->
                match integratorInitial with
                | Some v -> sprintf "x0 = %s" (fmt v)
                | None -> "x0 = ?"

            | "gain" ->
                match constantValue with
                | Some v -> sprintf "k = %s" (fmt v)
                | None -> "k = ?"

            | "add" ->
                "A + B"

            | "subtract" ->
                "A - B"

            | "multiply" ->
                "A × B"

            | "constraint" ->
                "Constraint node"

            | _ ->
                ""

        setTextIfExists "ParamText" paramText

    // ---------------------------
    // Double-click dialog
    // ---------------------------
    let openParamDialog () =
        let k = normalizeKind kind

        if k <> "constant" && k <> "integrator" && k <> "gain" then
            ()
        else
            let win = Window()
            win.Width <- 320
            win.Height <- 170
            win.CanResize <- false
            win.WindowStartupLocation <- WindowStartupLocation.CenterOwner
            win.Background <- SolidColorBrush(Color.Parse("#1f1f1f"))

            win.Title <-
                if k = "constant" then "Edit Constant"
                elif k = "gain" then "Edit Gain"
                else "Edit Integrator Initial"

            let root = StackPanel()
            root.Spacing <- 10.0
            root.Margin <- Thickness(14.0)

            let hint = TextBlock()

            hint.Text <-
                if k = "constant" then
                    "Enter constant value (use dot for decimals, e.g. 3.5)"
                elif k = "gain" then
                    "Enter gain factor k (use dot for decimals, e.g. 2.5)"
                else
                    "Enter integrator initial value x0 (use dot for decimals, e.g. 0.0)"

            hint.TextWrapping <- TextWrapping.Wrap
            hint.Foreground <- Brushes.White

            let tb = TextBox()
            tb.Height <- 32.0
            tb.Watermark <- "number"

            tb.Text <-
                if k = "constant" || k = "gain" then
                    (constantValue |> Option.defaultValue 0.0).ToString(Globalization.CultureInfo.InvariantCulture)
                else
                    (integratorInitial |> Option.defaultValue 0.0).ToString(Globalization.CultureInfo.InvariantCulture)

            let err = TextBlock()
            err.Text <- ""
            err.Foreground <- Brushes.OrangeRed

            let btnRow = StackPanel()
            btnRow.Orientation <- Orientation.Horizontal
            btnRow.Spacing <- 10.0
            btnRow.HorizontalAlignment <- HorizontalAlignment.Right

            let btnCancel = Button()
            btnCancel.Content <- "Cancel"
            btnCancel.MinWidth <- 90.0

            let btnOk = Button()
            btnOk.Content <- "OK"
            btnOk.MinWidth <- 90.0

            btnRow.Children.Add(btnCancel) |> ignore
            btnRow.Children.Add(btnOk) |> ignore

            root.Children.Add(hint) |> ignore
            root.Children.Add(tb) |> ignore
            root.Children.Add(err) |> ignore
            root.Children.Add(btnRow) |> ignore

            win.Content <- root

            btnCancel.Click.Add(fun _ -> win.Close())

            let apply () =
                match tryParseFloat tb.Text with
                | None ->
                    err.Text <- "Invalid number. Use dot for decimals (e.g. 3.5)."
                | Some v ->
                    if k = "constant" || k = "gain" then
                        constantValue <- Some v
                    else
                        integratorInitial <- Some v

                    refreshText ()
                    win.Close()

            btnOk.Click.Add(fun _ -> apply ())

            tb.KeyDown.Add(fun e ->
                if e.Key = Key.Enter then
                    e.Handled <- true
                    apply ()
            )

            match TopLevel.GetTopLevel(this) with
            | :? Window as owner ->
                win.ShowDialog(owner) |> ignore
            | _ ->
                win.Show()

    do
        AvaloniaXamlLoader.Load(this)

        match tryFindBorder "OutputPort" with
        | Some outP -> hookHandledClick (outP :> Control) (fun () -> outputPortClicked.Trigger(this))
        | None -> ()

        match tryFindBorder "InputPort1" with
        | Some in1 -> hookHandledClick (in1 :> Control) (fun () -> inputPort1Clicked.Trigger(this))
        | None -> ()

        match tryFindBorder "InputPort2" with
        | Some in2 -> hookHandledClick (in2 :> Control) (fun () -> inputPort2Clicked.Trigger(this))
        | None -> ()

        match tryFindBorder "Body" with
        | Some body -> body.DoubleTapped.Add(fun _ -> openParamDialog ())
        | None -> ()

        refreshText ()

    // ---------------------------
    // API used by MainWindow
    // ---------------------------
    member _.OutputPortClicked = outputPortClicked.Publish
    member _.InputPort1Clicked = inputPort1Clicked.Publish
    member _.InputPort2Clicked = inputPort2Clicked.Publish

    member _.SetTitle(title: string) =
        setTextIfExists "TitleText" title

    member _.SetOutputHighlight(onOff: bool) =
        match tryFindBorder "OutputPort" with
        | Some b -> setBorderHighlight b onOff Brushes.Gold
        | None -> ()
        
    member _.SetBodyVisual(backgroundHex: string, borderHex: string) =
        let body = this.FindControl<Border>("Body")
        if not (isNull body) then
            body.Background <- SolidColorBrush(Color.Parse(backgroundHex))
            body.BorderBrush <- SolidColorBrush(Color.Parse(borderHex))

        let accent = this.FindControl<Border>("AccentBar")
        if not (isNull accent) then
            accent.Background <- SolidColorBrush(Color.Parse(borderHex))

    member _.SetInputHighlight(port: InputPortId, onOff: bool) =
        match port with
        | In1 ->
            match tryFindBorder "InputPort1" with
            | Some b -> setBorderHighlight b onOff Brushes.OrangeRed
            | None -> ()
        | In2 ->
            match tryFindBorder "InputPort2" with
            | Some b -> setBorderHighlight b onOff Brushes.OrangeRed
            | None -> ()

    // ---------------------------
    // Properties
    // ---------------------------
    member _.NodeId
        with get () = nodeId
        and set v = nodeId <- v

    member _.Kind
        with get () = kind
        and set v =
            kind <- (if isNull v then "" else v)
            refreshText ()

    member _.Constant
        with get () = constantValue
        and set v =
            constantValue <- v
            refreshText ()

    member _.IntegratorInitial
        with get () = integratorInitial
        and set v =
            integratorInitial <- v
            refreshText ()

    member _.Refresh() = refreshText ()
    member _.OpenEditDialog() = openParamDialog ()