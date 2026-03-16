namespace SimulinkClone.Core.Simulation

open System

module ConstraintGraphValidation =

    type ConstraintGraphError =
        | DuplicateBlockId of ConstraintBlockId
        | UnknownFromBlock of ConstraintBlockId
        | UnknownToBlock of ConstraintBlockId
        | UnknownFromTerminal of ConstraintBlockId * TerminalName
        | UnknownToTerminal of ConstraintBlockId * TerminalName
        | MultipleWiresToSameTerminal of ConstraintBlockId * TerminalName
        | SelfLoop of ConstraintBlockId * TerminalName * TerminalName
        | InvalidConstantConfiguration of ConstraintBlockId
        | InvalidGainConfiguration of ConstraintBlockId
        | UnknownKnownValueBlock of ConstraintBlockId
        | UnknownKnownValueTerminal of ConstraintBlockId * TerminalName
        | DuplicateKnownValueForTerminal of ConstraintBlockId * TerminalName

    type ConstraintGraphValidation =
        { Ok: bool
          Errors: string list }

    let private errorToString =
        function
        | DuplicateBlockId id ->
            $"Duplicate block id '{id}'."

        | UnknownFromBlock id ->
            $"Wire references unknown From block '{id}'."

        | UnknownToBlock id ->
            $"Wire references unknown To block '{id}'."

        | UnknownFromTerminal (blockId, terminal) ->
            $"Wire references unknown terminal '{terminal}' on block '{blockId}' as FromRef."

        | UnknownToTerminal (blockId, terminal) ->
            $"Wire references unknown terminal '{terminal}' on block '{blockId}' as ToRef."

        | MultipleWiresToSameTerminal (blockId, terminal) ->
            $"Block '{blockId}' terminal '{terminal}' already has a wire."

        | SelfLoop (blockId, fromTerminal, toTerminal) ->
            $"Self-loop is not allowed on block '{blockId}' ({fromTerminal} -> {toTerminal})."

        | InvalidConstantConfiguration blockId ->
            $"Constant block '{blockId}' must define only terminal 'Result' and have ConstantValue."

        | InvalidGainConfiguration blockId ->
            $"Gain block '{blockId}' must define terminals 'A' and 'Result' and have ConstantValue."

        | UnknownKnownValueBlock blockId ->
            $"Known value references unknown block '{blockId}'."

        | UnknownKnownValueTerminal (blockId, terminal) ->
            $"Known value references unknown terminal '{terminal}' on block '{blockId}'."

        | DuplicateKnownValueForTerminal (blockId, terminal) ->
            $"Multiple known values are defined for block '{blockId}' terminal '{terminal}'."

    let private terminalNames (block: ConstraintBlock) =
        block.Terminals |> List.map (fun t -> t.Name)

    let private hasTerminal (terminalName: TerminalName) (block: ConstraintBlock) =
        block.Terminals |> List.exists (fun t -> t.Name = terminalName)

    let private sameElements (expected: string list) (actual: string list) =
        let e = expected |> Set.ofList
        let a = actual |> Set.ofList
        e = a

    let validate (graph: ConstraintGraph) : ConstraintGraphValidation =
        let errors = ResizeArray<ConstraintGraphError>()

        let duplicateIds =
            graph.Blocks
            |> List.groupBy (fun b -> b.Id)
            |> List.choose (fun (id, xs) ->
                if List.length xs > 1 then Some id else None)

        for id in duplicateIds do
            errors.Add(DuplicateBlockId id)

        let blockMap =
            graph.Blocks
            |> List.map (fun b -> b.Id, b)
            |> Map.ofList

        for block in graph.Blocks do
            match block.Kind with
            | ConstraintBlockKind.Constant ->
                let names = terminalNames block
                if block.ConstantValue.IsNone || not (sameElements [ "Result" ] names) then
                    errors.Add(InvalidConstantConfiguration block.Id)

            | ConstraintBlockKind.Gain ->
                let names = terminalNames block
                if block.ConstantValue.IsNone || not (sameElements [ "A"; "Result" ] names) then
                    errors.Add(InvalidGainConfiguration block.Id)

            | ConstraintBlockKind.Add
            | ConstraintBlockKind.Subtract
            | ConstraintBlockKind.Multiply ->
                ()

        for wire in graph.Wires do
            if wire.FromRef.BlockId = wire.ToRef.BlockId then
                errors.Add(SelfLoop (wire.FromRef.BlockId, wire.FromRef.Terminal, wire.ToRef.Terminal))

            match blockMap |> Map.tryFind wire.FromRef.BlockId with
            | None ->
                errors.Add(UnknownFromBlock wire.FromRef.BlockId)
            | Some fromBlock ->
                if not (hasTerminal wire.FromRef.Terminal fromBlock) then
                    errors.Add(UnknownFromTerminal (wire.FromRef.BlockId, wire.FromRef.Terminal))

            match blockMap |> Map.tryFind wire.ToRef.BlockId with
            | None ->
                errors.Add(UnknownToBlock wire.ToRef.BlockId)
            | Some toBlock ->
                if not (hasTerminal wire.ToRef.Terminal toBlock) then
                    errors.Add(UnknownToTerminal (wire.ToRef.BlockId, wire.ToRef.Terminal))

        let duplicateInputs =
            graph.Wires
            |> List.groupBy (fun w -> w.ToRef.BlockId, w.ToRef.Terminal)
            |> List.choose (fun ((blockId, terminal), xs) ->
                if List.length xs > 1 then Some (blockId, terminal) else None)

        for (blockId, terminal) in duplicateInputs do
            errors.Add(MultipleWiresToSameTerminal (blockId, terminal))

        for known in graph.KnownValues do
            match blockMap |> Map.tryFind known.Ref.BlockId with
            | None ->
                errors.Add(UnknownKnownValueBlock known.Ref.BlockId)
            | Some block ->
                if not (hasTerminal known.Ref.Terminal block) then
                    errors.Add(UnknownKnownValueTerminal (known.Ref.BlockId, known.Ref.Terminal))

        let duplicateKnowns =
            graph.KnownValues
            |> List.groupBy (fun kv -> kv.Ref.BlockId, kv.Ref.Terminal)
            |> List.choose (fun ((blockId, terminal), xs) ->
                if List.length xs > 1 then Some (blockId, terminal) else None)

        for (blockId, terminal) in duplicateKnowns do
            errors.Add(DuplicateKnownValueForTerminal (blockId, terminal))

        let errorStrings =
            errors |> Seq.map errorToString |> Seq.toList

        { Ok = errorStrings.IsEmpty
          Errors = errorStrings }