module SimulationTests

open Xunit
open SimulinkClone.Core.Simulation

let assertClose (expected: float) (actual: float) (eps: float) =
    Assert.InRange(actual, expected - eps, expected + eps)

[<Fact>]
let ``Integrator ramp grows linearly`` () =
    let dt = 0.1
    let steps = 5
    let samples = Engine.demoIntegratorRamp dt steps

    let ys = samples |> List.map (fun s -> s.value)

    // prvi je 0 (y = x prije update-a)
    assertClose 0.0 ys.[0] 1e-12

    // očekivanja za dt=0.1 i u=3:
    // y1 = 0.3, y2 = 0.6 ...
    assertClose 0.3 ys.[1] 1e-9
    assertClose 0.6 ys.[2] 1e-9