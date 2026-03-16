namespace SimulinkClone.Core.Simulation

open SimulinkClone.Core.Simulation.Graph

module ConstraintGraphSolver =

    type NodeValue =
        | Known of float
        | Unknown

    type NodeStatus =
        | Ok
        | Underdetermined
        | Error

    type SolveResult =
        { Values: Map<NodeId, NodeValue>
          Statuses: Map<NodeId, NodeStatus> }

    let private approxEqual (a: float) (b: float) =
        abs (a - b) < 0.0001

    let private tryKnown nodeId (values: Map<NodeId, NodeValue>) =
        match values |> Map.tryFind nodeId with
        | Some (Known v) -> Some v
        | _ -> None

    let private setKnown nodeId value (values: Map<NodeId, NodeValue>) =
        values |> Map.add nodeId (Known value)

    let private incomingTo (graph: Graph) (nodeId: NodeId) =
        graph.edges |> List.filter (fun e -> e.toId = nodeId)

    let private sourceForPort (graph: Graph) (nodeId: NodeId) (port: int) =
        graph.edges
        |> List.tryFind (fun e -> e.toId = nodeId && e.toPort = port)
        |> Option.map (fun e -> e.fromId)

    let private evaluateNode (graph: Graph) (values: Map<NodeId, NodeValue>) (node: Node) =
        match node.kind with
        | Constant ->
            match node.constant with
            | Some c ->
                let values2 = setKnown node.id c values
                values2, Ok
            | None ->
                values, Error

        | Gain ->
            let in1 = sourceForPort graph node.id 1 |> Option.bind (fun id -> tryKnown id values)
            match node.constant, in1 with
            | Some k, Some x ->
                let values2 = setKnown node.id (k * x) values
                values2, Ok
            | Some _, None ->
                values, Underdetermined
            | None, _ ->
                values, Error

        | Add ->
            let a = sourceForPort graph node.id 1 |> Option.bind (fun id -> tryKnown id values)
            let b = sourceForPort graph node.id 2 |> Option.bind (fun id -> tryKnown id values)

            match a, b with
            | Some x, Some y ->
                let values2 = setKnown node.id (x + y) values
                values2, Ok
            | _ ->
                values, Underdetermined

        | Subtract ->
            let a = sourceForPort graph node.id 1 |> Option.bind (fun id -> tryKnown id values)
            let b = sourceForPort graph node.id 2 |> Option.bind (fun id -> tryKnown id values)

            match a, b with
            | Some x, Some y ->
                let values2 = setKnown node.id (x - y) values
                values2, Ok
            | _ ->
                values, Underdetermined

        | Multiply ->
            let a = sourceForPort graph node.id 1 |> Option.bind (fun id -> tryKnown id values)
            let b = sourceForPort graph node.id 2 |> Option.bind (fun id -> tryKnown id values)

            match a, b with
            | Some x, Some y ->
                let values2 = setKnown node.id (x * y) values
                values2, Ok
            | _ ->
                values, Underdetermined

        | Integrator ->
            values, Underdetermined

    let solveForward (graph: Graph) =
        let validation = Graph.validate graph
        if not validation.ok then
            failwith (String.concat " | " validation.errors)

        let initialValues =
            graph.nodes
            |> List.map (fun n -> n.id, Unknown)
            |> Map.ofList

        let mutable values = initialValues
        let mutable statuses = Map.empty<NodeId, NodeStatus>

        for node in graph.nodes do
            let newValues, status = evaluateNode graph values node
            values <- newValues
            statuses <- statuses |> Map.add node.id status

        { Values = values
          Statuses = statuses }