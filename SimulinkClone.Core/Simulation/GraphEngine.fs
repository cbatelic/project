namespace SimulinkClone.Core.Simulation

open System
open SimulinkClone.Core.Simulation

type EvalError =
    | GraphInvalid of string list
    | MissingInput of NodeId * InputPort
    | UnsupportedBlock of NodeId * BlockKind
    | UnknownOutputNode of NodeId

type EvalResult<'T> =
    | Ok of 'T
    | Error of EvalError

module GraphEngine =

    let private portOfInt =
        function
        | 1 -> Some In1
        | 2 -> Some In2
        | _ -> None

    let private portInt =
        function
        | In1 -> 1
        | In2 -> 2

    /// helper: find source for a given toId+port
    let private tryGetInput (g: Graph) (toId: NodeId) (p: InputPort) =
        let pInt = portInt p
        g.edges
        |> List.tryFind (fun e -> e.toId = toId && e.toPort = pInt)
        |> Option.map (fun e -> e.fromId)

    /// topo order (assumes Graph.validate already ok, i.e. DAG + ports ok)
    let private topoOrder (g: Graph) : NodeId list =
        let incoming =
            g.nodes
            |> List.map (fun n ->
                let inc =
                    g.edges
                    |> List.filter (fun e -> e.toId = n.id)
                    |> List.choose (fun e -> portOfInt e.toPort |> Option.map (fun _ -> e.fromId))
                n.id, Set.ofList inc)
            |> Map.ofList

        let outgoing =
            g.nodes
            |> List.map (fun n ->
                let outs =
                    g.edges
                    |> List.filter (fun e -> e.fromId = n.id)
                    |> List.choose (fun e -> portOfInt e.toPort |> Option.map (fun _ -> e.toId))
                n.id, Set.ofList outs)
            |> Map.ofList

        let mutable incMap = incoming
        let q = Collections.Generic.Queue<NodeId>()

        for KeyValue(id, inc) in incMap do
            if inc.Count = 0 then q.Enqueue(id)

        let mutable order = []
        while q.Count > 0 do
            let n = q.Dequeue()
            order <- n :: order
            for m in outgoing[n] do
                let incM = incMap[m].Remove(n)
                incMap <- incMap.Add(m, incM)
                if incM.Count = 0 then q.Enqueue(m)

        List.rev order

    /// evaluira graf u jednoj točki (bez vremena) -> output vrijednosti po nodeId
    /// (Integrator nije podržan ovdje)
    let evalOnce (g: Graph) : EvalResult<Map<NodeId, float>> =
        let v = Graph.validate g
        if not v.ok then
            Error(GraphInvalid v.errors)
        else
            let nodeById = g.nodes |> List.map (fun n -> n.id, n) |> Map.ofList
            let order = topoOrder g

            let mutable values : Map<NodeId, float> = Map.empty

            let inline getValue id = values[id]

            let evalNode (id: NodeId) =
                let n = nodeById[id]
                match n.kind with
                | Constant ->
                    n.constant.Value
                | Add ->
                    let aSrc = tryGetInput g id In1
                    let bSrc = tryGetInput g id In2
                    match aSrc, bSrc with
                    | Some aId, Some bId -> (getValue aId) + (getValue bId)
                    | None, _ -> raise (ArgumentException($"Missing input In1 for node {id}"))
                    | _, None -> raise (ArgumentException($"Missing input In2 for node {id}"))
                | Integrator ->
                    raise (NotSupportedException($"Integrator not supported in evalOnce (node {id})."))

            try
                for id in order do
                    let n = nodeById[id]
                    match n.kind with
                    | Integrator -> ()
                    | _ ->
                        let vv = evalNode id
                        values <- values.Add(id, vv)

                if g.nodes |> List.exists (fun n -> n.kind = Integrator) then
                    Error(UnsupportedBlock("?", Integrator))
                else
                    Ok values
            with
            | :? ArgumentException as ex ->
                Error(GraphInvalid [ ex.Message ])
            | :? NotSupportedException as ex ->
                Error(GraphInvalid [ ex.Message ])

    /// Simulacija grafa kroz vrijeme (Constant/Add/Integrator)
    /// Integrator:
    ///  - input: In1
    ///  - state init: node.constant (ako None -> 0.0)
    ///  - y = x (prije update-a)
    ///  - xNext = x + dt*u
    ///
    /// outputs: lista nodeId koje želiš kao series
    let runGraph (dt: Dt) (steps: int) (outputs: NodeId list) (g: Graph)
        : EvalResult<Map<NodeId, Sample<float> list>> =

        let v = Graph.validate g
        if not v.ok then
            Error(GraphInvalid v.errors)
        else
            let outputs = outputs |> List.distinct

            // outputs must exist
            let nodeIds = g.nodes |> List.map (fun n -> n.id) |> Set.ofList
            match outputs |> List.tryFind (fun id -> not (nodeIds.Contains id)) with
            | Some bad -> Error(UnknownOutputNode bad)
            | None ->

                let nodeById = g.nodes |> List.map (fun n -> n.id, n) |> Map.ofList
                let order = topoOrder g

                // state for integrators: NodeId -> x
                let mutable state : Map<NodeId, float> =
                    g.nodes
                    |> List.choose (fun n ->
                        if n.kind = Integrator then
                            Some (n.id, defaultArg n.constant 0.0)
                        else None)
                    |> Map.ofList

                // series buffers
                let series =
                    outputs
                    |> List.map (fun id -> id, ResizeArray<Sample<float>>())
                    |> Map.ofList

                // one step eval
                let evalStep (t: Time) =
                    let mutable values : Map<NodeId, float> = Map.empty
                    let mutable nextState = state

                    let inline getValue id = values[id]

                    for id in order do
                        let n = nodeById[id]

                        match n.kind with
                        | Constant ->
                            let c = n.constant.Value
                            values <- values.Add(id, c)

                        | Add ->
                            let aSrc = tryGetInput g id In1
                            let bSrc = tryGetInput g id In2
                            match aSrc, bSrc with
                            | Some aId, Some bId ->
                                let vv = (getValue aId) + (getValue bId)
                                values <- values.Add(id, vv)
                            | None, _ ->
                                raise (ArgumentException($"Missing input In1 for node {id}"))
                            | _, None ->
                                raise (ArgumentException($"Missing input In2 for node {id}"))

                        | Integrator ->
                            // u from In1
                            let uSrc = tryGetInput g id In1
                            match uSrc with
                            | None ->
                                raise (ArgumentException($"Missing input In1 for Integrator node {id}"))
                            | Some uId ->
                                let u = getValue uId
                                let x = state |> Map.tryFind id |> Option.defaultValue 0.0
                                let y = x
                                let xNext = x + dt * u
                                values <- values.Add(id, y)
                                nextState <- nextState.Add(id, xNext)

                    // commit state for next step
                    state <- nextState

                    // emit outputs
                    for outId in outputs do
                        let y = values[outId]
                        series[outId].Add({ t = t; value = y })

                try
                    for k in 0 .. (steps - 1) do
                        let t = float k * dt
                        evalStep t

                    let out =
                        series
                        |> Map.map (fun _ ra -> List.ofSeq ra)

                    Ok out
                with
                | :? ArgumentException as ex ->
                    Error(GraphInvalid [ ex.Message ])
                | :? NotSupportedException as ex ->
                    Error(GraphInvalid [ ex.Message ])