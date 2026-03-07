
namespace SimulinkClone.App

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Media
open Avalonia.Layout
open Avalonia.Controls.Shapes
open Avalonia.Collections

open SimulinkClone.App.Api

module PlotWindow =

    let private clamp01 (x: float) =
        if x < 0.0 then 0.0
        elif x > 1.0 then 1.0
        else x

    let private palette =
        [ "#facc15"  // yellow
          "#22c55e"  // green
          "#38bdf8"  // sky
          "#f472b6"  // pink
          "#fb923c"  // orange
          "#a78bfa"  // violet
          "#f87171"  // red
          "#4ade80" ] // emerald

    let private brushAt i =
        let c = palette.[i % palette.Length]
        SolidColorBrush(Color.Parse(c))

    let private safeMinMax (xs: float list) =
        match xs with
        | [] -> 0.0, 1.0
        | _ ->
            let mn = xs |> List.min
            let mx = xs |> List.max
            if abs (mx - mn) < 1e-9 then mn - 1.0, mx + 1.0 else mn, mx

    let show (owner: Window option) (titleText: string) (series: RunSeriesDto list) =
        let win = Window()
        win.Title <- titleText
        win.Width <- 950
        win.Height <- 560
        win.WindowStartupLocation <- WindowStartupLocation.CenterOwner
        win.Background <- SolidColorBrush(Color.Parse("#0f0f0f"))

        let root = Grid()
        root.RowDefinitions <- RowDefinitions("Auto,*")

        let header = DockPanel()
        header.Margin <- Thickness(14.0, 12.0, 14.0, 10.0)

        let title = TextBlock()
        title.Text <- titleText
        title.FontSize <- 18.0
        title.FontWeight <- FontWeight.SemiBold
        title.Foreground <- Brushes.White
        DockPanel.SetDock(title, Dock.Left)

        let hint = TextBlock()
        hint.Text <- "Multiple outputs"
        hint.Foreground <- SolidColorBrush(Color.Parse("#a7a7a7"))
        hint.VerticalAlignment <- VerticalAlignment.Center
        DockPanel.SetDock(hint, Dock.Right)

        header.Children.Add(title) |> ignore
        header.Children.Add(hint) |> ignore

        let plotHost = Border()
        plotHost.Margin <- Thickness(14.0, 0.0, 14.0, 14.0)
        plotHost.CornerRadius <- CornerRadius(12.0)
        plotHost.BorderThickness <- Thickness(1.0)
        plotHost.BorderBrush <- SolidColorBrush(Color.Parse("#2b2b2b"))
        plotHost.Background <- SolidColorBrush(Color.Parse("#111111"))

        let canvas = Canvas()
        canvas.Background <- Brushes.Transparent
        plotHost.Child <- canvas

        Grid.SetRow(header, 0)
        Grid.SetRow(plotHost, 1)
        root.Children.Add(header) |> ignore
        root.Children.Add(plotHost) |> ignore

        win.Content <- root

        let draw () =
            canvas.Children.Clear()

            if List.isEmpty series then
                let msg = TextBlock(Text = "No series returned.", Foreground = Brushes.OrangeRed)
                Canvas.SetLeft(msg, 20.0)
                Canvas.SetTop(msg, 20.0)
                canvas.Children.Add(msg) |> ignore
            else
                let w = max 10.0 canvas.Bounds.Width
                let h = max 10.0 canvas.Bounds.Height

                let padL, padR, padT, padB = 55.0, 20.0, 38.0, 42.0
                let innerW = max 10.0 (w - padL - padR)
                let innerH = max 10.0 (h - padT - padB)

                let axisBrush = SolidColorBrush(Color.Parse("#3a3a3a"))

                // axes
                let xAxis = Line(Stroke = axisBrush, StrokeThickness = 1.0)
                xAxis.StartPoint <- Point(padL, padT + innerH)
                xAxis.EndPoint <- Point(padL + innerW, padT + innerH)

                let yAxis = Line(Stroke = axisBrush, StrokeThickness = 1.0)
                yAxis.StartPoint <- Point(padL, padT)
                yAxis.EndPoint <- Point(padL, padT + innerH)

                canvas.Children.Add(xAxis) |> ignore
                canvas.Children.Add(yAxis) |> ignore

                // all samples for bounds
                let allSamples = series |> List.collect (fun s -> s.samples)
                let ts = allSamples |> List.map (fun p -> p.t)
                let vs = allSamples |> List.map (fun p -> p.value)

                let tMin, tMax = safeMinMax ts
                let vMin, vMax = safeMinMax vs

                let toX (t: float) =
                    let u = (t - tMin) / (tMax - tMin) |> clamp01
                    padL + u * innerW

                let toY (v: float) =
                    let u = (v - vMin) / (vMax - vMin) |> clamp01
                    padT + (1.0 - u) * innerH

                // grid lines
                for i in 1 .. 4 do
                    let y = padT + (float i / 5.0) * innerH
                    let grid = Line(Stroke = SolidColorBrush(Color.Parse("#1f1f1f")), StrokeThickness = 1.0)
                    grid.StartPoint <- Point(padL, y)
                    grid.EndPoint <- Point(padL + innerW, y)
                    canvas.Children.Add(grid) |> ignore

                // draw all series
                series
                |> List.iteri (fun i s ->
                    let poly = Polyline()
                    poly.Stroke <- brushAt i
                    poly.StrokeThickness <- 2.0
                    poly.Points <- AvaloniaList<Point>()

                    for p in s.samples do
                        poly.Points.Add(Point(toX p.t, toY p.value))

                    canvas.Children.Add(poly) |> ignore

                    // legend
                    let legend = TextBlock()
                    legend.Text <- 
                        if s.id.Contains("integrator") then "Integrator"
                        elif s.id.Contains("add") then "Add"
                        elif s.id.Contains("constant") then "Constant"
                        else s.id.Substring(0,8)
                    legend.Foreground <- brushAt i
                    legend.FontSize <- 12.0
                    Canvas.SetLeft(legend, padL + (float i * 120.0))
                    Canvas.SetTop(legend, 10.0)
                    canvas.Children.Add(legend) |> ignore
                )

                let info = TextBlock()
                info.Text <- sprintf "t=[%g..%g]  v=[%g..%g]" tMin tMax vMin vMax
                info.Foreground <- SolidColorBrush(Color.Parse("#a7a7a7"))
                info.FontSize <- 12.0
                Canvas.SetLeft(info, padL)
                Canvas.SetTop(info, h - 24.0)
                canvas.Children.Add(info) |> ignore

                let yMaxText = TextBlock()
                yMaxText.Text <- sprintf "%g" vMax
                yMaxText.Foreground <- SolidColorBrush(Color.Parse("#808080"))
                yMaxText.FontSize <- 11.0
                Canvas.SetLeft(yMaxText, 8.0)
                Canvas.SetTop(yMaxText, padT - 6.0)
                canvas.Children.Add(yMaxText) |> ignore

                let yMinText = TextBlock()
                yMinText.Text <- sprintf "%g" vMin
                yMinText.Foreground <- SolidColorBrush(Color.Parse("#808080"))
                yMinText.FontSize <- 11.0
                Canvas.SetLeft(yMinText, 8.0)
                Canvas.SetTop(yMinText, padT + innerH - 6.0)
                canvas.Children.Add(yMinText) |> ignore

        win.Opened.Add(fun _ -> draw())
        canvas.SizeChanged.Add(fun _ -> draw())

        match owner with
        | Some o -> win.ShowDialog(o) |> ignore
        | None -> win.Show()