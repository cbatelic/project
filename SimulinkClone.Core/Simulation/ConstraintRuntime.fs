namespace SimulinkClone.Core.Simulation

open SimulinkClone.Core.Simulation

open SimulinkClone.Core.Simulation.ConstraintGraphValidation

module ConstraintRuntime =

    type Value =
        | Known of float
        | Unknown

    type BlockStatus =
        | Ok
        | Underdetermined
        | Error

    type BlockRuntimeState =
        { Block: ConstraintBlock
          Values: Map<TerminalName, Value>
          Status: BlockStatus }

    type SolveResult =
        { Blocks: BlockRuntimeState list }

    let private approxEqual (a: float) (b: float) =
        abs (a - b) < 0.0001

    let private tryGetKnown (terminal: TerminalName) (values: Map<TerminalName, Value>) =
        match values |> Map.tryFind terminal with
        | Some (Known v) -> Some v
        | _ -> None

    let private setKnown (terminal: TerminalName) (value: float) (values: Map<TerminalName, Value>) =
        values |> Map.add terminal (Known value)

    let private getValueOrUnknown (terminal: TerminalName) (values: Map<TerminalName, Value>) =
        values |> Map.tryFind terminal |> Option.defaultValue Unknown

    let private createInitialState (block: ConstraintBlock) =
        let initialValues =
            block.Terminals
            |> List.map (fun t ->
                let initial =
                    match block.Kind, block.ConstantValue, t.Name with
                    | ConstraintBlockKind.Constant, Some c, "Result" -> Known c
                    | _ -> Unknown
                t.Name, initial)
            |> Map.ofList

        { Block = block
          Values = initialValues
          Status = Underdetermined }
    
    let initializeGraph (graph: ConstraintGraph) =
        graph.Blocks |> List.map createInitialState

    let private findBlockState (blockId: string) (states: BlockRuntimeState list) =
        states |> List.tryFind (fun s -> s.Block.Id = blockId)

    let private updateBlockState (blockId: string) (newValues: Map<TerminalName, Value>) (newStatus: BlockStatus) (states: BlockRuntimeState list) =
        states
        |> List.map (fun s ->
            if s.Block.Id = blockId then
                { s with Values = newValues; Status = newStatus }
            else
                s)

    let evaluateBlock (state: BlockRuntimeState) =
        let values = state.Values

        match state.Block.Kind with
        | ConstraintBlockKind.Constant ->
            match state.Block.ConstantValue with
            | Some c ->
                let values2 = values |> setKnown "Result" c
                { state with Values = values2; Status = Ok }
            | None ->
                { state with Status = Error }

        | ConstraintBlockKind.Add ->
            let a = tryGetKnown "A" values
            let b = tryGetKnown "B" values
            let r = tryGetKnown "Result" values

            match a, b, r with
            | Some av, Some bv, None ->
                { state with Values = values |> setKnown "Result" (av + bv); Status = Ok }

            | Some av, None, Some rv ->
                { state with Values = values |> setKnown "B" (rv - av); Status = Ok }

            | None, Some bv, Some rv ->
                { state with Values = values |> setKnown "A" (rv - bv); Status = Ok }

            | Some av, Some bv, Some rv ->
                if approxEqual (av + bv) rv then
                    { state with Status = Ok }
                else
                    { state with Status = Error }

            | _ ->
                { state with Status = Underdetermined }

        | ConstraintBlockKind.Subtract ->
            let a = tryGetKnown "A" values
            let b = tryGetKnown "B" values
            let r = tryGetKnown "Result" values

            match a, b, r with
            | Some av, Some bv, None ->
                { state with Values = values |> setKnown "Result" (av - bv); Status = Ok }

            | Some av, None, Some rv ->
                { state with Values = values |> setKnown "B" (av - rv); Status = Ok }

            | None, Some bv, Some rv ->
                { state with Values = values |> setKnown "A" (rv + bv); Status = Ok }

            | Some av, Some bv, Some rv ->
                if approxEqual (av - bv) rv then
                    { state with Status = Ok }
                else
                    { state with Status = Error }

            | _ ->
                { state with Status = Underdetermined }

        | ConstraintBlockKind.Multiply ->
            let a = tryGetKnown "A" values
            let b = tryGetKnown "B" values
            let r = tryGetKnown "Result" values

            match a, b, r with
            | Some av, Some bv, None ->
                { state with Values = values |> setKnown "Result" (av * bv); Status = Ok }

            | Some av, None, Some rv ->
                if approxEqual av 0.0 then
                    { state with Status = Error }
                else
                    { state with Values = values |> setKnown "B" (rv / av); Status = Ok }

            | None, Some bv, Some rv ->
                if approxEqual bv 0.0 then
                    { state with Status = Error }
                else
                    { state with Values = values |> setKnown "A" (rv / bv); Status = Ok }

            | Some av, Some bv, Some rv ->
                if approxEqual (av * bv) rv then
                    { state with Status = Ok }
                else
                    { state with Status = Error }

            | _ ->
                { state with Status = Underdetermined }

        | ConstraintBlockKind.Gain ->
            let a = tryGetKnown "A" values
            let r = tryGetKnown "Result" values

            match state.Block.ConstantValue, a, r with
            | Some k, Some av, None ->
                { state with Values = values |> setKnown "Result" (k * av); Status = Ok }

            | Some k, None, Some rv ->
                if approxEqual k 0.0 then
                    { state with Status = Error }
                else
                    { state with Values = values |> setKnown "A" (rv / k); Status = Ok }

            | Some k, Some av, Some rv ->
                if approxEqual (k * av) rv then
                    { state with Status = Ok }
                else
                    { state with Status = Error }

            | _ ->
                { state with Status = Underdetermined }

    let propagateWire (wire: ConstraintWire) (states: BlockRuntimeState list) =
        let fromBlock = findBlockState wire.FromRef.BlockId states
        let toBlock = findBlockState wire.ToRef.BlockId states

        match fromBlock, toBlock with
        | Some fb, Some tb ->
            let fromValue = getValueOrUnknown wire.FromRef.Terminal fb.Values
            let toValue = getValueOrUnknown wire.ToRef.Terminal tb.Values

            match fromValue, toValue with
            | Known v, Unknown ->
                let updatedValues = tb.Values |> Map.add wire.ToRef.Terminal (Known v)
                updateBlockState tb.Block.Id updatedValues tb.Status states

            | Known v1, Known v2 ->
                if approxEqual v1 v2 then
                    states
                else
                    let updatedStates =
                        states
                        |> List.map (fun s ->
                            if s.Block.Id = tb.Block.Id then
                                { s with Status = Error }
                            else
                                s)
                    updatedStates

            | _ ->
                states

        | _ ->
            states
            
    let private applyKnownValue (known: KnownTerminalValue) (states: BlockRuntimeState list) =
        states
        |> List.map (fun s ->
            if s.Block.Id = known.Ref.BlockId then
                { s with
                    Values = s.Values |> Map.add known.Ref.Terminal (Known known.Value) }
            else
                s)

    let private applyKnownValues (knownValues: KnownTerminalValue list) (states: BlockRuntimeState list) =
        knownValues |> List.fold (fun acc kv -> applyKnownValue kv acc) states

    let propagateAllWires (wires: ConstraintWire list) (states: BlockRuntimeState list) =
        wires |> List.fold (fun acc wire -> propagateWire wire acc) states

    let private sameState (left: BlockRuntimeState list) (right: BlockRuntimeState list) =
        List.length left = List.length right
        && List.forall2 (fun a b ->
            a.Block.Id = b.Block.Id
            && a.Values = b.Values
            && a.Status = b.Status) left right

    let solve (graph: ConstraintGraph) =
        let validation = ConstraintGraphValidation.validate graph

        if not validation.Ok then
            failwith (String.concat " | " validation.Errors)

        let rec loop (current: BlockRuntimeState list) iteration =
            if iteration > 20 then
                current
            else
                let evaluated =
                    current |> List.map evaluateBlock

                let propagated =
                    propagateAllWires graph.Wires evaluated

                if sameState current propagated then
                    propagated
                else
                    loop propagated (iteration + 1)

        let initial =
            initializeGraph graph
            |> applyKnownValues graph.KnownValues

        { Blocks = loop initial 0 }     
