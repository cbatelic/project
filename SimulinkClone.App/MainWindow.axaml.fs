namespace SimulinkClone.App

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Markup.Xaml
open Avalonia.Media
open Avalonia.Controls.Shapes
open Avalonia.Layout
open SimulinkClone.App.Controls

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

type MainWindow() as this =
    inherit Window()

    let mutable nextX = 60.0
    let mutable nextY = 60.0

    let mutable dragging: BlockControl option = None
    let mutable dragOffset = Point(0, 0)

    let connections = ResizeArray<Connection>()

    let mutable sourceOutput: PortRef option = None
    let mutable tempConn: TempConnection option = None

    let mutable selectedBlock: BlockControl option = None

    let initXaml () =
        AvaloniaXamlLoader.Load(this) |> ignore

    let setOutput (text: string) =
        let tb = this.FindControl<TextBox>("TxtOutput")
        tb.Text <- text

    let clearInspector () =
        let host = this.FindControl<StackPanel>("InspectorHost")
        host.Children.Clear()

    // ---------- Inspector UI helpers ----------
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
            host.Children.Add(mkLabel "Summation") |> ignore
            host.Children.Add(mkRow "Operation" "In1 + In2") |> ignore
            host.Children.Add(mkMuted "Kasnije: izbor znakova (+/-) kao u Simulinku.") |> ignore

        | "constraint" ->
            host.Children.Add(mkLabel "Constraint") |> ignore
            host.Children.Add(mkRow "Mode" "Clamp") |> ignore
            host.Children.Add(mkRow "Min" "-") |> ignore
            host.Children.Add(mkRow "Max" "-") |> ignore
            host.Children.Add(mkMuted "Kasnije: Min/Max polja + validacija.") |> ignore

        | _ ->
            host.Children.Add(mkMuted "Nema UI definiran za ovaj blok još.") |> ignore

    // ---------- Geometry helpers ----------
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
            | "integrator" ->
                b.SetTitle("Integrator")
                b.IntegratorInitial <- Some 0.0
                b.Constant <- None
            | "constraint" ->
                b.SetTitle("Constraint")
                b.Constant <- None
                b.IntegratorInitial <- None
            | _ ->
                b.SetTitle(kind)

            Canvas.SetLeft(b, nextX)
            Canvas.SetTop(b, nextY)
            nextX <- nextX + 30.0
            nextY <- nextY + 30.0

            // ✅ FIX: double click opens dialog, single click drags
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

            // connect logic
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

            // auto-select new block
            selectBlock b

        // Toolbox buttons
        this.FindControl<Button>("BtnConstant").Click.Add(fun _ -> addBlock "constant")
        this.FindControl<Button>("BtnAdd").Click.Add(fun _ -> addBlock "add")
        this.FindControl<Button>("BtnIntegrator").Click.Add(fun _ -> addBlock "integrator")
        this.FindControl<Button>("BtnConstraint").Click.Add(fun _ -> addBlock "constraint")

        // Move dragging + temp connection
        canvas.PointerMoved.Add(fun args ->
            let p = args.GetPosition(canvas)

            match dragging with
            | Some b ->
                Canvas.SetLeft(b, p.X - dragOffset.X)
                Canvas.SetTop(b, p.Y - dragOffset.Y)
                updateAllConnectionsForBlock canvas b

                // update inspector while dragging selected block
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

        // Click empty canvas -> clear selection (+ cancel connect if active)
        canvas.PointerPressed.Add(fun args ->
            if obj.ReferenceEquals(args.Source, canvas) then
                selectedBlock <- None
                clearInspector()
                setOutput ""
                match sourceOutput with
                | None -> ()
                | Some _ -> cancelConnect canvas
        )