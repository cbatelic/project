namespace SimulinkClone.App

open System
open System.Threading.Tasks
open System.Globalization
open System.Collections.Generic

open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Markup.Xaml
open Avalonia.Media
open Avalonia.Controls.Shapes
open Avalonia.Layout

open SimulinkClone.App.Controls
open SimulinkClone.App.Api
open SimulinkClone.App.Api.ServiceClient

type PortRef =
    { Block: BlockControl
      Input: InputPortId option }

type Connection =
    { From: PortRef
      To_: PortRef
      Path: Path
      Geom: PathGeometry
      Fig: PathFigure
      Seg: BezierSegment
      Arrow: Path
      ArrowGeom: PathGeometry
      ArrowFig: PathFigure
      ArrowSeg1: LineSegment
      ArrowSeg2: LineSegment }

type TempConnection =
    { From: PortRef
      ToPos: Point
      Path: Path
      Geom: PathGeometry
      Fig: PathFigure
      Seg: BezierSegment
      Arrow: Path
      ArrowGeom: PathGeometry
      ArrowFig: PathFigure
      ArrowSeg1: LineSegment
      ArrowSeg2: LineSegment }
    
type ConstraintKnownEditor =
    { mutable A: float option
      mutable B: float option
      mutable Result: float option }

type MainWindow() as this =
    inherit Window()

    let BASE_URL = "http://localhost:5256/"

    let mutable nextX = 60.0
    let mutable nextY = 60.0

    let mutable dragging: BlockControl option = None
    let mutable dragOffset = Point(0, 0)

    let connections = ResizeArray<Connection>()

    let mutable sourceOutput: PortRef option = None
    let mutable tempConn: TempConnection option = None

    let mutable selectedBlock: BlockControl option = None
    let constraintKnowns = Dictionary<string, ConstraintKnownEditor>()

    let initXaml () =
        AvaloniaXamlLoader.Load(this) |> ignore

    let setOutput (text: string) =
        let tb = this.FindControl<TextBox>("TxtOutput")
        tb.Text <- text

    let clearInspector () =
        let host = this.FindControl<StackPanel>("InspectorHost")
        host.Children.Clear()

    let mkLabel (text: string) =
        let t = TextBlock()
        t.Text <- text
        t.Foreground <- Brushes.White
        t.FontWeight <- FontWeight.SemiBold
        t

    let mkMuted (text: string) =
        let t = TextBlock()
        t.Text <- text
        t.Foreground <- SolidColorBrush(Color.Parse("#a7a7a7"))
        t.TextWrapping <- TextWrapping.Wrap
        t

    let mkRow (label: string) (value: string) =
        let row = Grid()
        row.ColumnDefinitions <- ColumnDefinitions("120,*")

        let l = TextBlock()
        l.Text <- label
        l.Foreground <- SolidColorBrush(Color.Parse("#bdbdbd"))
        Grid.SetColumn(l, 0)

        let v = TextBlock()
        v.Text <- value
        v.Foreground <- Brushes.White
        Grid.SetColumn(v, 1)

        row.Children.Add(l) |> ignore
        row.Children.Add(v) |> ignore
        row
        
    let tryParseOptionalFloat (text: string) =
        let t = if isNull text then "" else text.Trim()

        if String.IsNullOrWhiteSpace(t) then
            None
        else
            match Double.TryParse(t, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, v -> Some v
            | false, _ -> None

    let formatOptionalFloat =
        function
        | Some v -> sprintf "%g" v
        | None -> ""

    let getOrCreateConstraintKnownEditor (blockId: string) =
        match constraintKnowns.TryGetValue(blockId) with
        | true, editor -> editor
        | false, _ ->
            let editor =
                { A = None
                  B = None
                  Result = None }
            constraintKnowns[blockId] <- editor
            editor

    let mkInputRow (label: string) (initialValue: string) (onChanged: string -> unit) =
        let row = Grid()
        row.ColumnDefinitions <- ColumnDefinitions("120,*")
        row.Margin <- Thickness(0.0, 2.0, 0.0, 2.0)

        let l = TextBlock()
        l.Text <- label
        l.Foreground <- SolidColorBrush(Color.Parse("#bdbdbd"))
        l.VerticalAlignment <- VerticalAlignment.Center
        Grid.SetColumn(l, 0)

        let tb = TextBox()
        tb.Text <- initialValue
        tb.Watermark <- "leave empty = unknown"
        tb.MinWidth <- 120.0
        tb.HorizontalAlignment <- HorizontalAlignment.Stretch
        tb.TextChanged.Add(fun _ -> onChanged tb.Text)
        Grid.SetColumn(tb, 1)

        row.Children.Add(l) |> ignore
        row.Children.Add(tb) |> ignore
        row

    let kindOf (b: BlockControl) =
        let k = b.Kind
        if isNull k then "" else k.Trim().ToLowerInvariant()

    let showInspectorForBlock (canvas: Canvas) (b: BlockControl) =
        let host = this.FindControl<StackPanel>("InspectorHost")
        host.Children.Clear()

        let k = kindOf b
        let x = Canvas.GetLeft(b)
        let y = Canvas.GetTop(b)

        host.Children.Add(mkLabel "Block") |> ignore
        host.Children.Add(mkRow "Kind" (if k = "" then "(unknown)" else k)) |> ignore
        host.Children.Add(mkRow "Id" b.NodeId) |> ignore
        host.Children.Add(mkRow "X" (sprintf "%.1f" x)) |> ignore
        host.Children.Add(mkRow "Y" (sprintf "%.1f" y)) |> ignore

        let sep = Separator()
        sep.Margin <- Thickness(0.0, 6.0, 0.0, 6.0)
        host.Children.Add(sep) |> ignore

        match k with
        | "constant" ->
            host.Children.Add(mkLabel "Constant") |> ignore
            let cTxt =
                match b.Constant with
                | Some v -> sprintf "%g" v
                | None -> "-"
            host.Children.Add(mkRow "Value (C)" cTxt) |> ignore
            host.Children.Add(mkMuted "Double-click block to edit (dialog).") |> ignore

        | "integrator" ->
            host.Children.Add(mkLabel "Integrator") |> ignore
            let x0Txt =
                match b.IntegratorInitial with
                | Some v -> sprintf "%g" v
                | None -> "-"
            host.Children.Add(mkRow "Initial (x0)" x0Txt) |> ignore
            host.Children.Add(mkMuted "Double-click block to edit (dialog).") |> ignore

        | "add" ->
            let editor = getOrCreateConstraintKnownEditor b.NodeId

            host.Children.Add(mkLabel "Add") |> ignore
            host.Children.Add(mkRow "Operation" "A + B = Result") |> ignore
            host.Children.Add(mkMuted "Unesi poznate vrijednosti. Prazno = unknown.") |> ignore
            host.Children.Add(mkInputRow "A" (formatOptionalFloat editor.A) (fun txt -> editor.A <- tryParseOptionalFloat txt)) |> ignore
            host.Children.Add(mkInputRow "B" (formatOptionalFloat editor.B) (fun txt -> editor.B <- tryParseOptionalFloat txt)) |> ignore
            host.Children.Add(mkInputRow "Result" (formatOptionalFloat editor.Result) (fun txt -> editor.Result <- tryParseOptionalFloat txt)) |> ignore
        | "subtract" ->
            let editor = getOrCreateConstraintKnownEditor b.NodeId

            host.Children.Add(mkLabel "Subtract") |> ignore
            host.Children.Add(mkRow "Operation" "A - B = Result") |> ignore
            host.Children.Add(mkMuted "Unesi poznate vrijednosti. Prazno = unknown.") |> ignore
            host.Children.Add(mkInputRow "A" (formatOptionalFloat editor.A) (fun txt -> editor.A <- tryParseOptionalFloat txt)) |> ignore
            host.Children.Add(mkInputRow "B" (formatOptionalFloat editor.B) (fun txt -> editor.B <- tryParseOptionalFloat txt)) |> ignore
            host.Children.Add(mkInputRow "Result" (formatOptionalFloat editor.Result) (fun txt -> editor.Result <- tryParseOptionalFloat txt)) |> ignore
        | "multiply" ->
            let editor = getOrCreateConstraintKnownEditor b.NodeId

            host.Children.Add(mkLabel "Multiply") |> ignore
            host.Children.Add(mkRow "Operation" "A * B = Result") |> ignore
            host.Children.Add(mkMuted "Unesi poznate vrijednosti. Prazno = unknown.") |> ignore
            host.Children.Add(mkInputRow "A" (formatOptionalFloat editor.A) (fun txt -> editor.A <- tryParseOptionalFloat txt)) |> ignore
            host.Children.Add(mkInputRow "B" (formatOptionalFloat editor.B) (fun txt -> editor.B <- tryParseOptionalFloat txt)) |> ignore
            host.Children.Add(mkInputRow "Result" (formatOptionalFloat editor.Result) (fun txt -> editor.Result <- tryParseOptionalFloat txt)) |> ignore
        | "constraint" ->
            host.Children.Add(mkLabel "Constraint") |> ignore
            host.Children.Add(mkRow "Mode" "Clamp") |> ignore
            host.Children.Add(mkRow "Min" "-") |> ignore
            host.Children.Add(mkRow "Max" "-") |> ignore
            host.Children.Add(mkMuted "Kasnije: Min/Max polja + validacija.") |> ignore

        | "gain" ->
            let editor = getOrCreateConstraintKnownEditor b.NodeId

            host.Children.Add(mkLabel "Gain") |> ignore
            let kTxt =
                match b.Constant with
                | Some v -> sprintf "%g" v
                | None -> "-"
            host.Children.Add(mkRow "Factor (k)" kTxt) |> ignore
            host.Children.Add(mkRow "Operation" "k * A = Result") |> ignore
            host.Children.Add(mkMuted "Gain factor se uzima iz bloka, a ovdje unosiš poznate terminal vrijednosti.") |> ignore
            host.Children.Add(mkInputRow "A" (formatOptionalFloat editor.A) (fun txt -> editor.A <- tryParseOptionalFloat txt)) |> ignore
            host.Children.Add(mkInputRow "Result" (formatOptionalFloat editor.Result) (fun txt -> editor.Result <- tryParseOptionalFloat txt)) |> ignore
        | _ ->
            host.Children.Add(mkMuted "Nema UI definiran za ovaj blok još.") |> ignore

    let isConstraintKind (kind: string) =
        match (if isNull kind then "" else kind.Trim().ToLowerInvariant()) with
        | "constant"
        | "add"
        | "subtract"
        | "multiply"
        | "gain" -> true
        | _ -> false

    let getConstraintKnownValues (b: BlockControl) =
        let k = kindOf b

        let fromEditor () =
            match constraintKnowns.TryGetValue(b.NodeId) with
            | false, _ -> []
            | true, editor ->
                match k with
                | "gain" ->
                    [ match editor.A with
                      | Some v ->
                          yield
                              { blockId = b.NodeId
                                terminal = "A"
                                value = v }
                      | None -> ()

                      match editor.Result with
                      | Some v ->
                          yield
                              { blockId = b.NodeId
                                terminal = "Result"
                                value = v }
                      | None -> () ]

                | "add"
                | "subtract"
                | "multiply" ->
                    [ match editor.A with
                      | Some v ->
                          yield
                              { blockId = b.NodeId
                                terminal = "A"
                                value = v }
                      | None -> ()

                      match editor.B with
                      | Some v ->
                          yield
                              { blockId = b.NodeId
                                terminal = "B"
                                value = v }
                      | None -> ()

                      match editor.Result with
                      | Some v ->
                          yield
                              { blockId = b.NodeId
                                terminal = "Result"
                                value = v }
                      | None -> () ]

                | _ -> []

        match k with
        | "constant" ->
            match b.Constant with
            | Some v ->
                [ { blockId = b.NodeId
                    terminal = "Result"
                    value = v } ]
            | None -> []

        | "gain"
        | "add"
        | "subtract"
        | "multiply" ->
            fromEditor ()

        | _ -> []
    let tryMapConnectionToConstraintWire (e: Connection) =
        let fromKind = kindOf e.From.Block
        let toKind = kindOf e.To_.Block

        if not (isConstraintKind fromKind) || not (isConstraintKind toKind) then
            None
        else
            let toTerminal =
                match e.To_.Input with
                | Some In1 -> "A"
                | Some In2 -> "B"
                | None -> "A"

            Some
                { fromBlockId = e.From.Block.NodeId
                  fromTerminal = "Result"
                  toBlockId = e.To_.Block.NodeId
                  toTerminal = toTerminal }

    let buildConstraintGraphDto (canvas: Canvas) =
        let blocks =
            canvas.Children
            |> Seq.choose (fun c ->
                match c with
                | :? BlockControl as b ->
                    let k = kindOf b
                    if isConstraintKind k then
                        Some
                            { id = b.NodeId
                              kind = k
                              constantValue =
                                match k with
                                | "constant" -> b.Constant
                                | "gain" -> b.Constant
                                | _ -> None }
                    else
                        None
                | _ -> None)
            |> Seq.toList

        let wires =
            connections
            |> Seq.choose tryMapConnectionToConstraintWire
            |> Seq.toList

        let knownValues =
            canvas.Children
            |> Seq.choose (fun c ->
                match c with
                | :? BlockControl as b ->
                    Some (getConstraintKnownValues b)
                | _ -> None)
            |> Seq.collect id
            |> Seq.toList

        { blocks = blocks
          wires = wires
          knownValues = knownValues }

    let formatConstraintResult (result: ConstraintRunResponseDto) =
        result.blocks
        |> List.map (fun b ->
            let terminals =
                b.terminals
                |> List.map (fun t ->
                    let valueText =
                        match t.value with
                        | Some v -> sprintf "%g" v
                        | None -> "?"
                    sprintf "  %s = %s" t.name valueText)
                |> String.concat Environment.NewLine

            sprintf "Block: %s\nStatus: %s\n%s" b.id b.status terminals)
        |> String.concat "\n\n"
    
    let getOutputPortPosition (block: BlockControl) (canvas: Canvas) =
        let port = block.FindControl<Border>("OutputPort")
        port.TranslatePoint(Point(6, 6), canvas).Value

    let getInputPortPosition (block: BlockControl) (portId: InputPortId) (canvas: Canvas) =
        let name =
            match portId with
            | In1 -> "InputPort1"
            | In2 -> "InputPort2"
        let port = block.FindControl<Border>(name)
        port.TranslatePoint(Point(6, 6), canvas).Value

    let clamp (v: float) (minV: float) (maxV: float) =
        Math.Max(minV, Math.Min(maxV, v))

    let updateBezierAndArrow
        (startP: Point)
        (endP: Point)
        (fig: PathFigure)
        (seg: BezierSegment)
        (arrowFig: PathFigure)
        (arrowSeg1: LineSegment)
        (arrowSeg2: LineSegment) =

        let dist = abs (endP.X - startP.X)
        let dx = clamp (dist * 0.6) 40.0 240.0

        let c1 = Point(startP.X + dx, startP.Y)
        let c2 = Point(endP.X - dx, endP.Y)

        fig.StartPoint <- startP
        seg.Point1 <- c1
        seg.Point2 <- c2
        seg.Point3 <- endP

        let dir = Point(endP.X - c2.X, endP.Y - c2.Y)
        let len = Math.Sqrt(dir.X * dir.X + dir.Y * dir.Y)
        let ux, uy =
            if len < 0.0001 then 1.0, 0.0 else dir.X / len, dir.Y / len

        let px, py = -uy, ux

        let arrowLen = 12.0
        let arrowWidth = 6.0

        let tip = endP
        let baseP = Point(endP.X - ux * arrowLen, endP.Y - uy * arrowLen)
        let pA = Point(baseP.X + px * arrowWidth, baseP.Y + py * arrowWidth)
        let pB = Point(baseP.X - px * arrowWidth, baseP.Y - py * arrowWidth)

        arrowFig.StartPoint <- tip
        arrowSeg1.Point <- pA
        arrowSeg2.Point <- pB

    let createBezierPath () =
        let geom = PathGeometry()
        let fig = PathFigure(IsClosed = false, IsFilled = false)
        let seg = BezierSegment()

        fig.Segments.Add(seg) |> ignore
        geom.Figures.Add(fig) |> ignore

        let path =
            Path(
                Data = geom,
                Stroke = Brushes.Gold,
                StrokeThickness = 2.0
            )
        path.IsHitTestVisible <- false

        let arrowGeom = PathGeometry()
        let arrowFig = PathFigure(IsClosed = true, IsFilled = true)
        let ls1 = LineSegment()
        let ls2 = LineSegment()
        arrowFig.Segments.Add(ls1) |> ignore
        arrowFig.Segments.Add(ls2) |> ignore
        arrowGeom.Figures.Add(arrowFig) |> ignore

        let arrow =
            Path(
                Data = arrowGeom,
                Fill = Brushes.Gold,
                Stroke = Brushes.Gold,
                StrokeThickness = 1.0
            )
        arrow.IsHitTestVisible <- false

        geom, fig, seg, path, arrowGeom, arrowFig, ls1, ls2, arrow

    let removeTemp (canvas: Canvas) =
        tempConn
        |> Option.iter (fun tc ->
            canvas.Children.Remove(tc.Path) |> ignore
            canvas.Children.Remove(tc.Arrow) |> ignore
        )
        tempConn <- None

    let cancelConnect (canvas: Canvas) =
        sourceOutput |> Option.iter (fun s -> s.Block.SetOutputHighlight(false))
        sourceOutput <- None
        removeTemp canvas

    let updateAllConnectionsForBlock (canvas: Canvas) (b: BlockControl) =
        for c in connections do
            if obj.ReferenceEquals(c.From.Block, b) || obj.ReferenceEquals(c.To_.Block, b) then
                let startP = getOutputPortPosition c.From.Block canvas
                let endP =
                    match c.To_.Input with
                    | Some pid -> getInputPortPosition c.To_.Block pid canvas
                    | None -> getOutputPortPosition c.To_.Block canvas
                updateBezierAndArrow startP endP c.Fig c.Seg c.ArrowFig c.ArrowSeg1 c.ArrowSeg2

    do
        initXaml()

        let canvas = this.FindControl<Canvas>("EditorCanvas")
        let client = ServiceClient(BASE_URL)

        let selectBlock (b: BlockControl) =
            selectedBlock <- Some b
            showInspectorForBlock canvas b
            setOutput (sprintf "Selected: %s (%s)" b.NodeId (kindOf b))

        let addBlock (kind: string) =
            let b = BlockControl()
            let k = kind.Trim().ToLowerInvariant()
            b.Kind <- k

            match k with
            | "constant" ->
                b.SetTitle("Constant")
                b.Constant <- Some 1.0
                b.IntegratorInitial <- None
            | "add" ->
                b.SetTitle("Add")
                b.Constant <- None
                b.IntegratorInitial <- None
            | "subtract" ->
                b.SetTitle("Subtract")
                b.Constant <- None
                b.IntegratorInitial <- None

            | "multiply" ->
                b.SetTitle("Multiply")
                b.Constant <- None
                b.IntegratorInitial <- None
            | "integrator" ->
                b.SetTitle("Integrator")
                b.IntegratorInitial <- Some 0.0
                b.Constant <- None
            | "constraint" ->
                b.SetTitle("Constraint")
                b.Constant <- None
                b.IntegratorInitial <- None
            | "gain" ->
                b.SetTitle("Gain")
                b.Constant <- Some 1.0
                b.IntegratorInitial <- None
            | _ ->
                b.SetTitle(kind)

            Canvas.SetLeft(b, nextX)
            Canvas.SetTop(b, nextY)
            nextX <- nextX + 30.0
            nextY <- nextY + 30.0

            b.PointerPressed.Add(fun args ->
                if not args.Handled then
                    selectBlock b

                    if args.ClickCount = 2 then
                        args.Handled <- true
                        b.OpenEditDialog()
                    else
                        dragging <- Some b
                        let p = args.GetPosition(canvas)
                        let left = Canvas.GetLeft(b)
                        let top = Canvas.GetTop(b)
                        dragOffset <- Point(p.X - left, p.Y - top)
                        args.Pointer.Capture(b) |> ignore
            )

            b.PointerReleased.Add(fun args ->
                if dragging.IsSome then
                    dragging <- None
                    args.Pointer.Capture(null) |> ignore
            )

            b.OutputPortClicked.Add(fun src ->
                cancelConnect canvas
                sourceOutput <- Some { Block = src; Input = None }
                src.SetOutputHighlight(true)

                let startP = getOutputPortPosition src canvas
                let geom, fig, seg, path, arrowGeom, arrowFig, ls1, ls2, arrow = createBezierPath()
                updateBezierAndArrow startP startP fig seg arrowFig ls1 ls2

                canvas.Children.Insert(0, path) |> ignore
                canvas.Children.Insert(0, arrow) |> ignore

                tempConn <-
                    Some
                        { From = { Block = src; Input = None }
                          ToPos = startP
                          Path = path
                          Geom = geom
                          Fig = fig
                          Seg = seg
                          Arrow = arrow
                          ArrowGeom = arrowGeom
                          ArrowFig = arrowFig
                          ArrowSeg1 = ls1
                          ArrowSeg2 = ls2 }
            )

            b.InputPort1Clicked.Add(fun target ->
                match sourceOutput, tempConn with
                | Some srcOut, Some tc ->
                    target.SetInputHighlight(In1, true)

                    let startP = getOutputPortPosition srcOut.Block canvas
                    let endP = getInputPortPosition target In1 canvas
                    updateBezierAndArrow startP endP tc.Fig tc.Seg tc.ArrowFig tc.ArrowSeg1 tc.ArrowSeg2

                    let conn =
                        { From = srcOut
                          To_ = { Block = target; Input = Some In1 }
                          Path = tc.Path
                          Geom = tc.Geom
                          Fig = tc.Fig
                          Seg = tc.Seg
                          Arrow = tc.Arrow
                          ArrowGeom = tc.ArrowGeom
                          ArrowFig = tc.ArrowFig
                          ArrowSeg1 = tc.ArrowSeg1
                          ArrowSeg2 = tc.ArrowSeg2 }

                    connections.Add(conn)

                    srcOut.Block.SetOutputHighlight(false)
                    target.SetInputHighlight(In1, false)
                    sourceOutput <- None
                    tempConn <- None
                | _ -> ()
            )

            b.InputPort2Clicked.Add(fun target ->
                match sourceOutput, tempConn with
                | Some srcOut, Some tc ->
                    target.SetInputHighlight(In2, true)

                    let startP = getOutputPortPosition srcOut.Block canvas
                    let endP = getInputPortPosition target In2 canvas
                    updateBezierAndArrow startP endP tc.Fig tc.Seg tc.ArrowFig tc.ArrowSeg1 tc.ArrowSeg2

                    let conn =
                        { From = srcOut
                          To_ = { Block = target; Input = Some In2 }
                          Path = tc.Path
                          Geom = tc.Geom
                          Fig = tc.Fig
                          Seg = tc.Seg
                          Arrow = tc.Arrow
                          ArrowGeom = tc.ArrowGeom
                          ArrowFig = tc.ArrowFig
                          ArrowSeg1 = tc.ArrowSeg1
                          ArrowSeg2 = tc.ArrowSeg2 }

                    connections.Add(conn)

                    srcOut.Block.SetOutputHighlight(false)
                    target.SetInputHighlight(In2, false)
                    sourceOutput <- None
                    tempConn <- None
                | _ -> ()
            )

            canvas.Children.Add(b) |> ignore
            selectBlock b

        this.FindControl<Button>("BtnConstant").Click.Add(fun _ -> addBlock "constant")
        this.FindControl<Button>("BtnAdd").Click.Add(fun _ -> addBlock "add")
        this.FindControl<Button>("BtnSubtract").Click.Add(fun _ -> addBlock "subtract")
        this.FindControl<Button>("BtnMultiply").Click.Add(fun _ -> addBlock "multiply")
        this.FindControl<Button>("BtnIntegrator").Click.Add(fun _ -> addBlock "integrator")
        this.FindControl<Button>("BtnConstraint").Click.Add(fun _ -> addBlock "constraint")
        this.FindControl<Button>("BtnGain").Click.Add(fun _ -> addBlock "gain")

        this.FindControl<Button>("BtnPing").Click.Add(fun _ ->
            task {
                try
                    let! res = client.GetHealthAsync()
                    setOutput res
                with ex ->
                    setOutput ("PING ERROR: " + ex.Message)
            } |> ignore
        )

        this.FindControl<Button>("BtnRun").Click.Add(fun _ ->
            task {
                try
                    let nodes =
                        canvas.Children
                        |> Seq.choose (fun c ->
                            match c with
                            | :? BlockControl as b ->
                                Some
                                    { id = b.NodeId
                                      kind = b.Kind
                                      constant =
                                        match kindOf b with
                                        | "constant" -> b.Constant
                                        | "gain" -> b.Constant
                                        | "integrator" -> b.IntegratorInitial
                                        | _ -> None
                                      x = Canvas.GetLeft(b)
                                      y = Canvas.GetTop(b) }
                            | _ -> None)
                        |> Seq.toList

                    let edges =
                        connections
                        |> Seq.map (fun e ->
                            { fromId = e.From.Block.NodeId
                              toId = e.To_.Block.NodeId
                              toPort =
                                match e.To_.Input with
                                | Some In1 -> 1
                                | Some In2 -> 2
                                | None -> 1 })
                        |> Seq.toList

                    let graph: UiGraphDto = { nodes = nodes; edges = edges }

                    let! saved =
                        client.PostJsonAsync<UiGraphDto, SaveGraphResponse>("api/ui/graphs", graph)

                    if not saved.ok then
                        setOutput "SAVE ERROR: backend returned ok=false"
                    else
                        // ✅ vrati sve serije, ne samo jednu
                        let outputs =
                            nodes |> List.map (fun n -> n.id)

                        let req: RunSavedRequest =
                            { dt = 0.1
                              steps = 200
                              outputs = outputs }

                        let url = sprintf "api/ui/graphs/%s/run" saved.id
                        let! runRes = client.PostJsonAsync<RunSavedRequest, RunResponse>(url, req)

                        setOutput (sprintf "OK. SavedId=%s. Series=%d" saved.id runRes.series.Length)

                        let owner =
                            match TopLevel.GetTopLevel(this) with
                            | :? Window as w -> Some w
                            | _ -> None

                        PlotWindow.show owner "Simulation" runRes.series

                with ex ->
                    setOutput ("RUN ERROR: " + ex.Message)
            } |> ignore
        )
        
        this.FindControl<Button>("BtnConstraintRun").Click.Add(fun _ ->
            task {
                try
                    let graph = buildConstraintGraphDto canvas

                    let! result =
                        client.PostJsonAsync<ConstraintUiGraphDto, ConstraintRunResponseDto>(
                            "api/constraint/run",
                            graph
                        )

                    if result.ok then
                        setOutput (formatConstraintResult result)
                    else
                        setOutput "Constraint run returned ok=false."

                with ex ->
                    setOutput ("CONSTRAINT RUN ERROR: " + ex.Message)
            } |> ignore
        )

        canvas.PointerMoved.Add(fun args ->
            let p = args.GetPosition(canvas)

            match dragging with
            | Some b ->
                Canvas.SetLeft(b, p.X - dragOffset.X)
                Canvas.SetTop(b, p.Y - dragOffset.Y)
                updateAllConnectionsForBlock canvas b

                match selectedBlock with
                | Some sb when obj.ReferenceEquals(sb, b) ->
                    showInspectorForBlock canvas sb
                | _ -> ()
            | None -> ()

            match sourceOutput, tempConn with
            | Some srcOut, Some tc ->
                let startP = getOutputPortPosition srcOut.Block canvas
                updateBezierAndArrow startP p tc.Fig tc.Seg tc.ArrowFig tc.ArrowSeg1 tc.ArrowSeg2
            | _ -> ()
        )

        canvas.PointerPressed.Add(fun args ->
            if obj.ReferenceEquals(args.Source, canvas) then
                selectedBlock <- None
                clearInspector()
                setOutput ""
                match sourceOutput with
                | None -> ()
                | Some _ -> cancelConnect canvas
        )