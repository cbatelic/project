namespace SimulinkClone.Core.Simulation

open System

type NodeId = string

type InputPort =
    | In1
    | In2

type BlockKind =
    | Constant
    | Add
    | Integrator

type Node =
    { id: NodeId
      kind: BlockKind
      constant: float option } 

type Edge =
    { fromId: NodeId
      toId: NodeId
      toPort: int } // 1 ili 2

type Graph =
    { nodes: Node list
      edges: Edge list }

type GraphError =
    | DuplicateNodeId of NodeId
    | UnknownFromNode of NodeId
    | UnknownToNode of NodeId
    | InvalidToPort of NodeId * int
    | MultipleWiresToSameInput of NodeId * int
    | SelfLoop of NodeId
    | CycleDetected of NodeId list
    | MissingConstantValue of NodeId

type GraphValidation =
    { ok: bool
      errors: string list }

module Graph =

    let private errToString =
        function
        | DuplicateNodeId id -> $"Duplicate node id '{id}'."
        | UnknownFromNode id -> $"Edge references unknown fromId '{id}'."
        | UnknownToNode id -> $"Edge references unknown toId '{id}'."
        | InvalidToPort (id, p) -> $"Node '{id}' has invalid toPort={p}. Allowed: 1 or 2."
        | MultipleWiresToSameInput (id, p) -> $"Node '{id}' input port {p} already has a connection."
        | SelfLoop id -> $"Self-loop is not allowed (node '{id}')."
        | CycleDetected ids ->
            let path = String.Join(" -> ", ids)
            $"Cycle detected: {path}."
        | MissingConstantValue id -> $"Constant node '{id}' is missing 'constant' value."

    let private toPort (p: int) =
        match p with
        | 1 -> Some In1
        | 2 -> Some In2
        | _ -> None

    let validate (g: Graph) : GraphValidation =
        let errors = ResizeArray<GraphError>()

        let dups =
            g.nodes
            |> List.groupBy (fun n -> n.id)
            |> List.choose (fun (id, xs) -> if List.length xs > 1 then Some id else None)

        for id in dups do
            errors.Add(DuplicateNodeId id)

        let nodeIds = g.nodes |> List.map (fun n -> n.id) |> Set.ofList

        for n in g.nodes do
            if n.kind = Constant && n.constant.IsNone then
                errors.Add(MissingConstantValue n.id)

        for e in g.edges do
            if e.fromId = e.toId then errors.Add(SelfLoop e.toId)
            if not (nodeIds.Contains e.fromId) then errors.Add(UnknownFromNode e.fromId)
            if not (nodeIds.Contains e.toId) then errors.Add(UnknownToNode e.toId)
            if toPort e.toPort |> Option.isNone then errors.Add(InvalidToPort (e.toId, e.toPort))

        let grouped =
            g.edges
            |> List.choose (fun e ->
                match toPort e.toPort with
                | None -> None
                | Some _ -> Some ((e.toId, e.toPort), e))
            |> List.groupBy fst

        for ((toId, port), xs) in grouped do
            if List.length xs > 1 then
                errors.Add(MultipleWiresToSameInput(toId, port))

        let incoming =
            g.nodes
            |> List.map (fun n ->
                let inc =
                    g.edges
                    |> List.filter (fun e -> e.toId = n.id)
                    |> List.choose (fun e -> toPort e.toPort |> Option.map (fun _ -> e.fromId))
                n.id, Set.ofList inc)
            |> Map.ofList

        let outgoing =
            g.nodes
            |> List.map (fun n ->
                let outs =
                    g.edges
                    |> List.filter (fun e -> e.fromId = n.id)
                    |> List.choose (fun e -> toPort e.toPort |> Option.map (fun _ -> e.toId))
                n.id, Set.ofList outs)
            |> Map.ofList

        let mutable incMap = incoming
        let q = System.Collections.Generic.Queue<NodeId>()
        for KeyValue(id, inc) in incMap do
            if inc.Count = 0 then q.Enqueue(id)

        let mutable processed = []

        while q.Count > 0 do
            let n = q.Dequeue()
            processed <- n :: processed
            let outs = outgoing[n]
            for m in outs do
                let incM = incMap[m].Remove(n)
                incMap <- incMap.Add(m, incM)
                if incM.Count = 0 then q.Enqueue(m)

        if processed.Length <> g.nodes.Length then
            let remaining =
                g.nodes
                |> List.map (fun n -> n.id)
                |> List.filter (fun id -> incMap[id].Count > 0)
            errors.Add(CycleDetected remaining)

        let strs = errors |> Seq.map errToString |> Seq.toList
        { ok = strs.IsEmpty; errors = strs }