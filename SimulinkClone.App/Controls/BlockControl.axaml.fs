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
    // Node state (for save/export/run later)
    // ---------------------------
    let mutable nodeId = Guid.NewGuid().ToString("N")
    let mutable kind = "constant" // "constant" | "add" | "integrator" | "constraint"
    let mutable constantValue: float option = Some 1.0
    let mutable integratorInitial: float option = Some 0.0

    // ---------------------------
    // UI helpers
    // ---------------------------
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

    // ✅ ICON MAP (no packages needed)
    let iconForKind (k: string) =
        match normalizeKind k with
        | "constant" -> "C"
        | "add" -> "∑"
        | "integrator" -> "∫"
        | "constraint" -> "⎇" // alternative: "⛔" or "⇔"
        | _ -> "■"

    // ---------------------------
    // Refresh title + icon + param text inside the block
    // Requires XAML: TextBlock Name="TitleIcon", Name="TitleText", Name="ParamText"
    // ---------------------------
    let refreshText () =
        let icon = this.FindControl<TextBlock>("TitleIcon")
        let title = this.FindControl<TextBlock>("TitleText")
        let param = this.FindControl<TextBlock>("ParamText")

        let k = normalizeKind kind

        // icon
        icon.Text <- iconForKind k

        // title
        title.Text <-
            match k with
            | "constant" -> "Constant"
            | "add" -> "Add"
            | "integrator" -> "Integrator"
            | "constraint" -> "Constraint"
            | other when other.Length > 0 -> other
            | _ -> "Block"

        // param line
        param.Text <-
            match k with
            | "constant" ->
                match constantValue with
                | Some v -> sprintf "C = %s" (fmt v)
                | None -> "C = ?"
            | "integrator" ->
                match integratorInitial with
                | Some v -> sprintf "x0 = %s" (fmt v)
                | None -> "x0 = ?"
            | "add" ->
                "" // summation shows no param for now
            | "constraint" ->
                "" // later: min/max etc.
            | _ ->
                ""

    // ---------------------------
    // Double-click on block body => edit param (constant / integrator)
    // ---------------------------
    let openParamDialog () =
        let k = normalizeKind kind

        if k <> "constant" && k <> "integrator" then
            ()
        else
            let win = Window()
            win.Width <- 320
            win.Height <- 170
            win.CanResize <- false
            win.WindowStartupLocation <- WindowStartupLocation.CenterOwner
            win.Background <- SolidColorBrush(Color.Parse("#1f1f1f"))
            win.Title <- if k = "constant" then "Edit Constant" else "Edit Integrator initial"

            let root = StackPanel()
            root.Spacing <- 10.0
            root.Margin <- Thickness(14.0)

            let hint = TextBlock()
            hint.Text <-
                if k = "constant" then
                    "Enter constant value (use dot for decimals, e.g. 3.5)"
                else
                    "Enter integrator initial value x0 (use dot for decimals, e.g. 0.0)"
            hint.TextWrapping <- TextWrapping.Wrap
            hint.Foreground <- Brushes.White

            let tb = TextBox()
            tb.Height <- 32.0
            tb.Watermark <- "number"
            tb.Text <-
                if k = "constant" then
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
                    if k = "constant" then constantValue <- Some v
                    else integratorInitial <- Some v
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

        let outP = this.FindControl<Border>("OutputPort")
        let in1 = this.FindControl<Border>("InputPort1")
        let in2 = this.FindControl<Border>("InputPort2")

        hookHandledClick (outP :> Control) (fun () -> outputPortClicked.Trigger(this))
        hookHandledClick (in1 :> Control) (fun () -> inputPort1Clicked.Trigger(this))
        hookHandledClick (in2 :> Control) (fun () -> inputPort2Clicked.Trigger(this))

        // Double-click on body => edit param (optional for now)
        let body = this.FindControl<Border>("Body")
        body.DoubleTapped.Add(fun _ -> openParamDialog())

        // initial display
        refreshText ()

    // ---------------------------
    // API used by MainWindow
    // ---------------------------
    member _.OutputPortClicked = outputPortClicked.Publish
    member _.InputPort1Clicked = inputPort1Clicked.Publish
    member _.InputPort2Clicked = inputPort2Clicked.Publish

    member _.SetTitle(title: string) =
        // keep it for backward compatibility, but Kind controls real title in refreshText
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

    // ---------------------------
    // Properties for export/save/run
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