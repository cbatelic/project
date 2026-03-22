module SimulationTests

open Xunit
open SimulinkClone.Core.Simulation

module CS = SimulinkClone.Core.Simulation.ConstraintSolver
module CGA = SimulinkClone.Core.Simulation.ConstraintGraphAdapter
module CR = SimulinkClone.Core.Simulation.ConstraintRuntime

module CGV = SimulinkClone.Core.Simulation.ConstraintGraphValidation

let assertClose (expected: float) (actual: float) (eps: float) =
    Assert.InRange(actual, expected - eps, expected + eps)

[<Fact>]
let ``Integrator ramp grows linearly`` () =
    let dt = 0.1
    let steps = 5
    let samples = Engine.demoIntegratorRamp dt steps

    let ys = samples |> List.map (fun s -> s.value)

    assertClose 0.0 ys.[0] 1e-12
    assertClose 0.3 ys.[1] 1e-9
    assertClose 0.6 ys.[2] 1e-9

[<Fact>]
let ``Constraint solver propagates values`` () =

    let sub : CS.BlockInstance =
        { Block = CS.subtractionBlock "sub1"
          Values =
              [ "A", CS.Known 50.0
                "B", CS.Known 18.0
                "Result", CS.Unknown ]
              |> Map.ofList
          Status = CS.Underdetermined }

    let mul : CS.BlockInstance =
        { Block = CS.multiplicationBlock "mul1"
          Values =
              [ "A", CS.Unknown
                "B", CS.Known 1.8
                "Result", CS.Known 57.6 ]
              |> Map.ofList
          Status = CS.Underdetermined }

    let connections : CS.Connection list =
        [ { From = { BlockId = "sub1"; Port = "Result" }
            To = { BlockId = "mul1"; Port = "A" } } ]

    let result =
        CS.runSolver [ sub; mul ] connections

    let subResult =
        result |> List.find (fun b -> b.Block.Id = "sub1")

    let mulResult =
        result |> List.find (fun b -> b.Block.Id = "mul1")

    match subResult.Values.["Result"] with
    | CS.Known v -> assertClose 32.0 v 1e-9
    | _ -> failwith "Result not computed"

    match mulResult.Values.["A"] with
    | CS.Known v -> assertClose 32.0 v 1e-9
    | _ -> failwith "Propagation failed"

[<Fact>]
let ``Constraint graph adapter solves simple network`` () =

    let graph : CGA.GraphModel =
        { Blocks =
            [ { Id = "sub1"
                BlockType = CGA.Subtraction
                Ports =
                    [ { Name = "A"; Value = Some 50.0 }
                      { Name = "B"; Value = Some 18.0 }
                      { Name = "Result"; Value = None } ] }

              { Id = "mul1"
                BlockType = CGA.Multiplication
                Ports =
                    [ { Name = "A"; Value = None }
                      { Name = "B"; Value = Some 1.8 }
                      { Name = "Result"; Value = Some 57.6 } ] } ]
          Connections =
            [ { FromBlockId = "sub1"
                FromPort = "Result"
                ToBlockId = "mul1"
                ToPort = "A" } ] }

    let solved = CGA.solveGraph graph

    let sub1 = solved.Blocks |> List.find (fun b -> b.Id = "sub1")
    let mul1 = solved.Blocks |> List.find (fun b -> b.Id = "mul1")

    let subResult =
        sub1.Ports |> List.find (fun p -> p.Name = "Result")

    let mulA =
        mul1.Ports |> List.find (fun p -> p.Name = "A")

    match subResult.Value with
    | Some v -> assertClose 32.0 v 1e-9
    | None -> failwith "sub1.Result was not solved"

    match mulA.Value with
    | Some v -> assertClose 32.0 v 1e-9
    | None -> failwith "mul1.A was not propagated"

    Assert.Equal("OK", mul1.Status)

[<Fact>]
let ``Constraint runtime solves subtraction then multiplication with wires`` () =
    let graph : ConstraintGraph =
        { Blocks =
            [ { Id = "far"
                Kind = ConstraintBlockKind.Constant
                ConstantValue = Some 50.0
                Terminals = [ { Name = "Result" } ] }

              { Id = "c18"
                Kind = ConstraintBlockKind.Constant
                ConstantValue = Some 18.0
                Terminals = [ { Name = "Result" } ] }

              { Id = "sub1"
                Kind = ConstraintBlockKind.Subtract
                ConstantValue = None
                Terminals = [ { Name = "A" }; { Name = "B" }; { Name = "Result" } ] }

              { Id = "k18"
                Kind = ConstraintBlockKind.Constant
                ConstantValue = Some 1.8
                Terminals = [ { Name = "Result" } ] }

              { Id = "mul1"
                Kind = ConstraintBlockKind.Multiply
                ConstantValue = None
                Terminals = [ { Name = "A" }; { Name = "B" }; { Name = "Result" } ] } ]
          Wires =
            [ { FromRef = { BlockId = "far"; Terminal = "Result" }
                ToRef = { BlockId = "sub1"; Terminal = "A" } }

              { FromRef = { BlockId = "c18"; Terminal = "Result" }
                ToRef = { BlockId = "sub1"; Terminal = "B" } }

              { FromRef = { BlockId = "sub1"; Terminal = "Result" }
                ToRef = { BlockId = "mul1"; Terminal = "A" } }

              { FromRef = { BlockId = "k18"; Terminal = "Result" }
                ToRef = { BlockId = "mul1"; Terminal = "B" } } ]
          KnownValues = [] }

    let result = CR.solve graph

    let sub1 = result.Blocks |> List.find (fun b -> b.Block.Id = "sub1")
    let mul1 = result.Blocks |> List.find (fun b -> b.Block.Id = "mul1")

    match sub1.Values.["Result"] with
    | CR.Known v -> assertClose 32.0 v 1e-9
    | CR.Unknown -> failwith "sub1.Result not solved"

    match mul1.Values.["Result"] with
    | CR.Known v -> assertClose 57.6 v 1e-9
    | CR.Unknown -> failwith "mul1.Result not solved"
    
[<Fact>]
let ``Constraint runtime solves from externally known values`` () =
    let graph : ConstraintGraph =
        { Blocks =
            [ { Id = "sub1"
                Kind = ConstraintBlockKind.Subtract
                ConstantValue = None
                Terminals = [ { Name = "A" }; { Name = "B" }; { Name = "Result" } ] }

              { Id = "mul1"
                Kind = ConstraintBlockKind.Multiply
                ConstantValue = None
                Terminals = [ { Name = "A" }; { Name = "B" }; { Name = "Result" } ] } ]
          Wires =
            [ { FromRef = { BlockId = "sub1"; Terminal = "Result" }
                ToRef = { BlockId = "mul1"; Terminal = "A" } } ]
          KnownValues =
            [ { Ref = { BlockId = "sub1"; Terminal = "A" }
                Value = 50.0 }

              { Ref = { BlockId = "sub1"; Terminal = "B" }
                Value = 18.0 }

              { Ref = { BlockId = "mul1"; Terminal = "B" }
                Value = 1.8 } ] }

    let result = CR.solve graph

    let sub1 = result.Blocks |> List.find (fun b -> b.Block.Id = "sub1")
    let mul1 = result.Blocks |> List.find (fun b -> b.Block.Id = "mul1")

    match sub1.Values.["Result"] with
    | CR.Known v -> assertClose 32.0 v 1e-9
    | CR.Unknown -> failwith "sub1.Result not solved"

    match mul1.Values.["A"] with
    | CR.Known v -> assertClose 32.0 v 1e-9
    | CR.Unknown -> failwith "mul1.A not propagated"

    match mul1.Values.["Result"] with
    | CR.Known v -> assertClose 57.6 v 1e-9
    | CR.Unknown -> failwith "mul1.Result not solved"
    
[<Fact>]
let ``Constraint graph validation fails for known value with unknown block`` () =
    let graph : ConstraintGraph =
        { Blocks =
            [ { Id = "sub1"
                Kind = ConstraintBlockKind.Subtract
                ConstantValue = None
                Terminals = [ { Name = "A" }; { Name = "B" }; { Name = "Result" } ] } ]
          Wires = []
          KnownValues =
            [ { Ref = { BlockId = "missing"; Terminal = "A" }
                Value = 10.0 } ] }

    let result = CGV.validate graph

    Assert.False(result.Ok)
    Assert.Contains(result.Errors, fun e -> e.Contains("Known value references unknown block"))


[<Fact>]
let ``Constraint graph validation fails for duplicate known values on same terminal`` () =
    let graph : ConstraintGraph =
        { Blocks =
            [ { Id = "mul1"
                Kind = ConstraintBlockKind.Multiply
                ConstantValue = None
                Terminals = [ { Name = "A" }; { Name = "B" }; { Name = "Result" } ] } ]
          Wires = []
          KnownValues =
            [ { Ref = { BlockId = "mul1"; Terminal = "A" }
                Value = 10.0 }

              { Ref = { BlockId = "mul1"; Terminal = "A" }
                Value = 20.0 } ] }

    let result = CGV.validate graph

    Assert.False(result.Ok)
    Assert.Contains(result.Errors, fun e -> e.Contains("Multiple known values are defined"))
    
[<Fact>]
let ``Constraint runtime keeps inconsistent values and marks error`` () =
    let graph : ConstraintGraph =
        { Blocks =
            [ { Id = "sub1"
                Kind = ConstraintBlockKind.Subtract
                ConstantValue = None
                Terminals = [ { Name = "A" }; { Name = "B" }; { Name = "Result" } ] } ]
          Wires = []
          KnownValues =
            [ { Ref = { BlockId = "sub1"; Terminal = "A" }
                Value = 50.0 }

              { Ref = { BlockId = "sub1"; Terminal = "B" }
                Value = 18.0 }

              { Ref = { BlockId = "sub1"; Terminal = "Result" }
                Value = 99.0 } ] }

    let result = CR.solve graph

    let sub1 = result.Blocks |> List.find (fun b -> b.Block.Id = "sub1")

    Assert.Equal(CR.Error, sub1.Status)

    match sub1.Values.["A"] with
    | CR.Known v -> assertClose 50.0 v 1e-9
    | CR.Unknown -> failwith "A should remain known"

    match sub1.Values.["B"] with
    | CR.Known v -> assertClose 18.0 v 1e-9
    | CR.Unknown -> failwith "B should remain known"

    match sub1.Values.["Result"] with
    | CR.Known v -> assertClose 99.0 v 1e-9
    | CR.Unknown -> failwith "Result should remain known"
    
[<Fact>]
let ``Constraint runtime marks block as underdetermined when too many values are missing`` () =
    let graph : ConstraintGraph =
        { Blocks =
            [ { Id = "sub1"
                Kind = ConstraintBlockKind.Subtract
                ConstantValue = None
                Terminals = [ { Name = "A" }; { Name = "B" }; { Name = "Result" } ] } ]
          Wires = []
          KnownValues =
            [ { Ref = { BlockId = "sub1"; Terminal = "A" }
                Value = 50.0 } ] }

    let result = CR.solve graph

    let sub1 = result.Blocks |> List.find (fun b -> b.Block.Id = "sub1")

    Assert.Equal(CR.Underdetermined, sub1.Status)

    match sub1.Values.["A"] with
    | CR.Known v -> assertClose 50.0 v 1e-9
    | CR.Unknown -> failwith "A should remain known"

    match sub1.Values.["B"] with
    | CR.Unknown -> ()
    | CR.Known _ -> failwith "B should remain unknown"

    match sub1.Values.["Result"] with
    | CR.Unknown -> ()
    | CR.Known _ -> failwith "Result should remain unknown"
    
[<Fact>]
let ``Constraint graph validation fails when source terminal is not Result`` () =
    let graph : ConstraintGraph =
        { Blocks =
            [ { Id = "sub1"
                Kind = ConstraintBlockKind.Subtract
                ConstantValue = None
                Terminals = [ { Name = "A" }; { Name = "B" }; { Name = "Result" } ] }

              { Id = "mul1"
                Kind = ConstraintBlockKind.Multiply
                ConstantValue = None
                Terminals = [ { Name = "A" }; { Name = "B" }; { Name = "Result" } ] } ]
          Wires =
            [ { FromRef = { BlockId = "sub1"; Terminal = "A" }
                ToRef = { BlockId = "mul1"; Terminal = "A" } } ]
          KnownValues = [] }

    let result = CGV.validate graph

    Assert.False(result.Ok)
    Assert.Contains(result.Errors, fun e -> e.Contains("source terminal must be 'Result'"))


[<Fact>]
let ``Constraint graph validation fails when target terminal is Result`` () =
    let graph : ConstraintGraph =
        { Blocks =
            [ { Id = "c1"
                Kind = ConstraintBlockKind.Constant
                ConstantValue = Some 10.0
                Terminals = [ { Name = "Result" } ] }

              { Id = "sub1"
                Kind = ConstraintBlockKind.Subtract
                ConstantValue = None
                Terminals = [ { Name = "A" }; { Name = "B" }; { Name = "Result" } ] } ]
          Wires =
            [ { FromRef = { BlockId = "c1"; Terminal = "Result" }
                ToRef = { BlockId = "sub1"; Terminal = "Result" } } ]
          KnownValues = [] }

    let result = CGV.validate graph

    Assert.False(result.Ok)
    Assert.Contains(result.Errors, fun e -> e.Contains("target terminal must be an input terminal"))


[<Fact>]
let ``Constraint graph validation fails when constant has incoming wire`` () =
    let graph : ConstraintGraph =
        { Blocks =
            [ { Id = "c1"
                Kind = ConstraintBlockKind.Constant
                ConstantValue = Some 10.0
                Terminals = [ { Name = "Result" } ] }

              { Id = "c2"
                Kind = ConstraintBlockKind.Constant
                ConstantValue = Some 20.0
                Terminals = [ { Name = "Result" } ] } ]
          Wires =
            [ { FromRef = { BlockId = "c1"; Terminal = "Result" }
                ToRef = { BlockId = "c2"; Terminal = "A" } } ]
          KnownValues = [] }

    let result = CGV.validate graph

    Assert.False(result.Ok)
    Assert.Contains(result.Errors, fun e ->
        e.Contains("cannot have incoming wires")
        || e.Contains("unknown terminal")
    )


[<Fact>]
let ``Constraint graph validation fails when gain receives B terminal`` () =
    let graph : ConstraintGraph =
        { Blocks =
            [ { Id = "c1"
                Kind = ConstraintBlockKind.Constant
                ConstantValue = Some 10.0
                Terminals = [ { Name = "Result" } ] }

              { Id = "g1"
                Kind = ConstraintBlockKind.Gain
                ConstantValue = Some 2.0
                Terminals = [ { Name = "A" }; { Name = "Result" } ] } ]
          Wires =
            [ { FromRef = { BlockId = "c1"; Terminal = "Result" }
                ToRef = { BlockId = "g1"; Terminal = "B" } } ]
          KnownValues = [] }

    let result = CGV.validate graph

    Assert.False(result.Ok)
    Assert.Contains(result.Errors, fun e -> e.Contains("unknown terminal 'B'") || e.Contains("cannot accept terminal 'B'"))


[<Fact>]
let ``Constraint graph validation passes for valid subtract to multiply chain`` () =
    let graph : ConstraintGraph =
        { Blocks =
            [ { Id = "c50"
                Kind = ConstraintBlockKind.Constant
                ConstantValue = Some 50.0
                Terminals = [ { Name = "Result" } ] }

              { Id = "c18"
                Kind = ConstraintBlockKind.Constant
                ConstantValue = Some 18.0
                Terminals = [ { Name = "Result" } ] }

              { Id = "sub1"
                Kind = ConstraintBlockKind.Subtract
                ConstantValue = None
                Terminals = [ { Name = "A" }; { Name = "B" }; { Name = "Result" } ] }

              { Id = "c1_8"
                Kind = ConstraintBlockKind.Constant
                ConstantValue = Some 1.8
                Terminals = [ { Name = "Result" } ] }

              { Id = "mul1"
                Kind = ConstraintBlockKind.Multiply
                ConstantValue = None
                Terminals = [ { Name = "A" }; { Name = "B" }; { Name = "Result" } ] } ]
          Wires =
            [ { FromRef = { BlockId = "c50"; Terminal = "Result" }
                ToRef = { BlockId = "sub1"; Terminal = "A" } }

              { FromRef = { BlockId = "c18"; Terminal = "Result" }
                ToRef = { BlockId = "sub1"; Terminal = "B" } }

              { FromRef = { BlockId = "sub1"; Terminal = "Result" }
                ToRef = { BlockId = "mul1"; Terminal = "A" } }

              { FromRef = { BlockId = "c1_8"; Terminal = "Result" }
                ToRef = { BlockId = "mul1"; Terminal = "B" } } ]
          KnownValues = [] }

    let result = CGV.validate graph

    Assert.True(result.Ok)
    Assert.Empty(result.Errors)