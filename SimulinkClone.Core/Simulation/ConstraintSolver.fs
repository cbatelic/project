namespace SimulinkClone.Core.Simulation

module ConstraintSolver =

    type Value =
        | Known of float
        | Unknown

    type BlockStatus =
        | Ok
        | Underdetermined
        | Error
        | Updated

    type PortName = string

    type PortValues = Map<PortName, Value>

    type ConstraintResult =
        { Values: PortValues
          Status: BlockStatus }

    type ConstraintBlock =
        { Id: string
          Ports: PortName list
          Evaluate: PortValues -> ConstraintResult }

    type Endpoint =
        { BlockId: string
          Port: PortName }

    type Connection =
        { From: Endpoint
          To: Endpoint }

    type BlockInstance =
        { Block: ConstraintBlock
          Values: PortValues
          Status: BlockStatus }

    let approxEqual a b =
        abs (a - b) < 0.0001

    let tryGetKnown name values =
        match values |> Map.tryFind name with
        | Some (Known v) -> Some v
        | _ -> None

    let setKnown name value values =
        values |> Map.add name (Known value)

    let subtractionBlock id =
        let ports = [ "A"; "B"; "Result" ]

        let evaluate values =
            let a = tryGetKnown "A" values
            let b = tryGetKnown "B" values
            let r = tryGetKnown "Result" values

            match a, b, r with
            | Some a, Some b, None ->
                { Values = values |> setKnown "Result" (a - b)
                  Status = Updated }

            | Some a, None, Some r ->
                { Values = values |> setKnown "B" (a - r)
                  Status = Updated }

            | None, Some b, Some r ->
                { Values = values |> setKnown "A" (r + b)
                  Status = Updated }

            | Some a, Some b, Some r ->
                if approxEqual (a - b) r then
                    { Values = values; Status = Ok }
                else
                    { Values = values; Status = Error }

            | _ ->
                { Values = values; Status = Underdetermined }

        { Id = id
          Ports = ports
          Evaluate = evaluate }

    let multiplicationBlock id =
        let ports = [ "A"; "B"; "Result" ]

        let evaluate values =
            let a = tryGetKnown "A" values
            let b = tryGetKnown "B" values
            let r = tryGetKnown "Result" values

            match a, b, r with
            | Some a, Some b, None ->
                { Values = values |> setKnown "Result" (a * b)
                  Status = Updated }

            | Some a, None, Some r ->
                { Values = values |> setKnown "B" (r / a)
                  Status = Updated }

            | None, Some b, Some r ->
                { Values = values |> setKnown "A" (r / b)
                  Status = Updated }

            | Some a, Some b, Some r ->
                if approxEqual (a * b) r then
                    { Values = values; Status = Ok }
                else
                    { Values = values; Status = Error }

            | _ ->
                { Values = values; Status = Underdetermined }

        { Id = id
          Ports = ports
          Evaluate = evaluate }

    let propagateConnection connection blocks =
        let findBlock id =
            blocks |> List.tryFind (fun b -> b.Block.Id = id)

        match findBlock connection.From.BlockId, findBlock connection.To.BlockId with
        | Some fb, Some tb ->
            let fromValue = fb.Values |> Map.tryFind connection.From.Port
            let toValue = tb.Values |> Map.tryFind connection.To.Port

            match fromValue, toValue with
            | Some (Known v), Some Unknown ->
                blocks
                |> List.map (fun b ->
                    if b.Block.Id = tb.Block.Id then
                        { b with
                            Values =
                                b.Values
                                |> Map.add connection.To.Port (Known v) }
                    else b)

            | _ -> blocks

        | _ -> blocks

    let propagateAllConnections connections blocks =
        connections |> List.fold (fun state conn -> propagateConnection conn state) blocks

    let private sameBlockState (a: BlockInstance) (b: BlockInstance) =
        a.Block.Id = b.Block.Id
        && a.Values = b.Values
        && a.Status = b.Status

    let private sameSolverState (left: BlockInstance list) (right: BlockInstance list) =
        List.length left = List.length right
        && List.forall2 sameBlockState left right

    let runSolver blocks connections =
        let rec loop current iteration =
            if iteration > 10 then
                current
            else
                let evaluated =
                    current
                    |> List.map (fun b ->
                        let result = b.Block.Evaluate b.Values
                        { b with
                            Values = result.Values
                            Status = result.Status })

                let propagated =
                    propagateAllConnections connections evaluated

                if sameSolverState propagated current then
                    propagated
                else
                    loop propagated (iteration + 1)

        loop blocks 0