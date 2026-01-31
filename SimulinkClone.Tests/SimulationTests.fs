module SimulationTests

open System
open Xunit
open SimulinkClone.Core.Simulation

[<Fact>]
let ``Integrator ramp grows linearly`` () =
    let dt = 0.1
    let steps = 5
    let samples = Engine.demoIntegratorRamp dt steps

    // u(t)=3, integrator Euler:
    // y0=0
    // x1=0+0.1*3=0.3 => y1=0.3 (ovisno gledaš y=x prije update-a; u našem je y=x prije update-a)
    // Naš y_k = x_k prije update-a:
    // t=0.0 y=0.0
    // t=0.1 y=0.3? -> ne, y je x prije update-a: x0=0 => y0=0; x1=0.3
    // t=0.1 y1=0.3; x2=0.6 ...
    let ys = samples |> List.map (fun s -> s.value)

    Assert.Equal<float>(0.0, ys.[0])
    Assert.Equal<float>(0.3, ys.[1], 10)
    Assert.Equal<float>(0.6, ys.[2], 10)
