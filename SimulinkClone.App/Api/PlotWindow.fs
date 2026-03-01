namespace SimulinkClone.App

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Media
open Avalonia.Controls.Shapes

open SimulinkClone.App.Api

module PlotWindow =

    let private clamp01 (x: float) =
        if x < 0.0 then 0.0 elif x > 1.0 then 1.0 else x

    let private nicePadding = 24.0

    let private fmt3 (v: float) =
        v.ToString("0.###", Globalization.CultureInfo.InvariantCulture)

    let show(owner: Window option, title: string, series: RunSeriesDto list) =
        let w = Window()
        w.Title <- title
        w.Width <- 860
        w.Height <- 520
        w.Background <- SolidColorBrush(Color.Parse("#111"))
        w.WindowStartupLocation <-
            match owner with
            | Some _ -> WindowStartupLocation.CenterOwner
            | None -> WindowStartupLocation.CenterScreen

        let root = Grid()
        root.RowDefinitions <- RowDefinitions("Auto,*")

        // Header
        let header = Border()
        header.Background <- SolidColorBrush(Color.Parse("#1a1a1a"))
        header.Padding <- Thickness(12)

        let headerText = TextBlock()
        headerText.Text <- title
        headerText.FontSize <- 16
        headerText.Foreground <- Brushes.White
        header.Child <- headerText

        // Plot canvas
        let plotHost = Border()
        plotHost.Background <- SolidColorBrush(Color.Parse("#0d0d0d"))
        plotHost.Padding <- Thickness(10)

        let canvas = Canvas()
        canvas.Background <- Brushes.Transparent
        plotHost.Child <- canvas

        Grid.SetRow(header, 0)
        Grid.SetRow(plotHost, 1)

        root.Children.Add(header) |> ignore
        root.Children.Add(plotHost) |> ignore
        w.Content <- root

        let draw() =
            canvas.Children.Clear()

            if series.IsEmpty then
                let t = TextBlock(Text = "No data", Foreground = Brushes.White)
                Canvas.SetLeft(t, 10)
                Canvas.SetTop(t, 10)
                canvas.Children.Add(t) |> ignore
            else
                // Flatten all points to compute bounds
                let allPts =
                    series
                    |> List.collect (fun s -> s.samples |> List.map (fun p -> p.t, p.value))

                if allPts.IsEmpty then
                    let t = TextBlock(Text = "No samples", Foreground = Brushes.White)
                    Canvas.SetLeft(t, 10)
                    Canvas.SetTop(t, 10)
                    canvas.Children.Add(t) |> ignore
                else
                    let ts = allPts |> List.map fst
                    let vs = allPts |> List.map snd

                    let tMin = ts |> List.min
                    let tMax = ts |> List.max
                    let vMin = vs |> List.min
                    let vMax = vs |> List.max

                    let width = max 10.0 canvas.Bounds.Width
                    let height = max 10.0 canvas.Bounds.Height

                    let x0 = nicePadding
                    let y0 = nicePadding
                    let wPlot = max 1.0 (width - nicePadding * 2.0)
                    let hPlot = max 1.0 (height - nicePadding * 2.0)

                    // axes
                    let axisX = Polyline()
                    axisX.Stroke <- SolidColorBrush(Color.Parse("#2a2a2a"))
                    axisX.StrokeThickness <- 1.0
                    axisX.Points <- Points([ Point(x0, y0 + hPlot); Point(x0 + wPlot, y0 + hPlot) ])
                    canvas.Children.Add(axisX) |> ignore

                    let axisY = Polyline()
                    axisY.Stroke <- SolidColorBrush(Color.Parse("#2a2a2a"))
                    axisY.StrokeThickness <- 1.0
                    axisY.Points <- Points([ Point(x0, y0); Point(x0, y0 + hPlot) ])
                    canvas.Children.Add(axisY) |> ignore

                    let scaleX (t: float) =
                        let denom = (tMax - tMin)
                        let u = if abs denom < 1e-9 then 0.0 else (t - tMin) / denom
                        x0 + clamp01 u * wPlot

                    let scaleY (v: float) =
                        let denom = (vMax - vMin)
                        let u = if abs denom < 1e-9 then 0.5 else (v - vMin) / denom
                        y0 + (1.0 - clamp01 u) * hPlot

                    // draw series
                    for s in series do
                        let line = Polyline()
                        line.Stroke <- SolidColorBrush(Color.Parse("#f4d03f")) // gold
                        line.StrokeThickness <- 2.0
                        line.IsHitTestVisible <- false

                        let pts = s.samples |> List.map (fun p -> Point(scaleX p.t, scaleY p.value))
                        line.Points <- Points(pts)
                        canvas.Children.Add(line) |> ignore

                    // legend
                    let legendText =
                        if series.Length = 1 then "Output"
                        else series |> List.map (fun s -> s.id) |> String.concat ", "

                    let legend =
                        TextBlock(
                            Text = legendText,
                            Foreground = SolidColorBrush(Color.Parse("#cfcfcf"))
                        )
                    Canvas.SetLeft(legend, x0)
                    Canvas.SetTop(legend, 6)
                    canvas.Children.Add(legend) |> ignore

                    // bounds label (SPRINTF umjesto interpolacije)
                    let boundsText =
                        sprintf "t=[%s..%s]  v=[%s..%s]"
                            (fmt3 tMin) (fmt3 tMax) (fmt3 vMin) (fmt3 vMax)

                    let lbl =
                        TextBlock(
                            Text = boundsText,
                            Foreground = SolidColorBrush(Color.Parse("#8f8f8f"))
                        )
                    Canvas.SetLeft(lbl, x0)
                    Canvas.SetTop(lbl, height - 18.0)
                    canvas.Children.Add(lbl) |> ignore

        w.Opened.Add(fun _ -> draw())
        canvas.SizeChanged.Add(fun _ -> draw())

        match owner with
        | Some o -> w.Show(o)
        | None -> w.Show()