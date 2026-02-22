namespace SimulinkClone.Service

open System
open System.IO
open System.Text.Json
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting

open SimulinkClone.Core.Simulation

// DTOs

type RampRequest = { dt: float; steps: int }
type SampleDto = { t: float; value: float }

type UiNodeDto =
    { id: string
      kind: string
      constant: float option
      x: float option
      y: float option }

type UiEdgeDto =
    { fromId: string
      toId: string
      toPort: int }

type UiGraphDto =
    { nodes: UiNodeDto list
      edges: UiEdgeDto list }

type RunSavedRequest =
    { dt: float
      steps: int
      outputs: string list }

type GraphMeta =
    { id: string
      createdAtUtc: DateTime
      updatedAtUtc: DateTime }

type StoredGraph =
    { meta: GraphMeta
      graph: UiGraphDto }

type GraphListItemDto =
    { id: string
      createdAtUtc: DateTime
      updatedAtUtc: DateTime
      nodeCount: int
      edgeCount: int }

module Program =

    let private jsonOptions =
        JsonSerializerOptions(
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            WriteIndented = true
        )

    // Storage path (repo folder)
    let private graphsDir =
        Path.Combine(Directory.GetCurrentDirectory(), "Data", "graphs")

    let private ensureStorage () =
        Directory.CreateDirectory(graphsDir) |> ignore

    let private graphPath (id: string) =
        Path.Combine(graphsDir, $"{id}.json")

    // Validation
    let private validateRamp (req: RampRequest) =
        [ if req.dt <= 0.0 then "dt must be > 0"
          if req.steps <= 0 then "steps must be >= 1"
          if req.steps > 10000 then "steps too large (max 10000)" ]

    let private validateRun (dt: float) (steps: int) (outputs: string list) =
        [ if dt <= 0.0 then "dt must be > 0"
          if steps <= 0 then "steps must be >= 1"
          if steps > 20000 then "steps too large (max 20000)"
          if outputs |> List.isEmpty then "outputs required" ]

    let private parseKind (s: string) : BlockKind option =
        match (if isNull s then "" else s).Trim().ToLowerInvariant() with
        | "constant" -> Some Constant
        | "add" -> Some Add
        | "integrator" -> Some Integrator
        | _ -> None

    let private uiToCoreGraph (dto: UiGraphDto) : Graph * string list =
        let errors = ResizeArray<string>()

        let nodes : Node list =
            dto.nodes
            |> List.choose (fun n ->
                match parseKind n.kind with
                | None ->
                    errors.Add($"Unknown node kind '{n.kind}' for node '{n.id}'.")
                    None
                | Some k ->
                    Some { id = n.id; kind = k; constant = n.constant })

        let edges : Edge list =
            dto.edges
            |> List.map (fun e ->
                { fromId = e.fromId
                  toId = e.toId
                  toPort = e.toPort })

        ({ nodes = nodes; edges = edges }, List.ofSeq errors)

    let private evalErrorToHttp (err: EvalError) : IResult =
        match err with
        | EvalError.GraphInvalid errs ->
            Results.BadRequest({| ok = false; errors = errs |})

        | EvalError.MissingInput (nodeId, port) ->
            let p = match port with | In1 -> 1 | In2 -> 2
            Results.BadRequest({| ok = false; errors = [ $"Missing input for node '{nodeId}' port {p}." ] |})

        | EvalError.UnsupportedBlock (nodeId, kind) ->
            Results.BadRequest({| ok = false; errors = [ $"Unsupported block '{kind}' at node '{nodeId}' (not implemented yet)." ] |})

        | EvalError.UnknownOutputNode nodeId ->
            Results.BadRequest({| ok = false; errors = [ $"Unknown output node '{nodeId}'." ] |})

    let private writeGraph (stored: StoredGraph) =
        ensureStorage()
        let json = JsonSerializer.Serialize(stored, jsonOptions)
        File.WriteAllText(graphPath stored.meta.id, json)

    let private tryReadGraph (id: string) : StoredGraph option =
        ensureStorage()
        let path = graphPath id
        if File.Exists(path) then
            let json = File.ReadAllText(path)
            Some (JsonSerializer.Deserialize<StoredGraph>(json, jsonOptions))
        else
            None

    let private listGraphs () : GraphListItemDto list =
        ensureStorage()

        Directory.GetFiles(graphsDir, "*.json")
        |> Array.choose (fun file ->
            try
                let json = File.ReadAllText(file)
                let g = JsonSerializer.Deserialize<StoredGraph>(json, jsonOptions)

                Some
                    { id = g.meta.id
                      createdAtUtc = g.meta.createdAtUtc
                      updatedAtUtc = g.meta.updatedAtUtc
                      nodeCount = g.graph.nodes.Length
                      edgeCount = g.graph.edges.Length }
            with _ ->
                None)
        |> Array.toList

    let private runStatelessSeries (g: Graph) (dt: float) (steps: int) (outputs: string list) : IResult =
        if g.nodes |> List.exists (fun n -> n.kind = Integrator) then
            Results.BadRequest({| ok = false; errors = [ "Integrator is not supported in /run yet (stateful execution not implemented)." ] |})
        else
            let distinctOutputs = outputs |> List.distinct
            let buffers =
                distinctOutputs
                |> List.map (fun id -> id, ResizeArray<SampleDto>())
                |> Map.ofList

            let mutable fail : IResult option = None

            for k in 0 .. (steps - 1) do
                if fail.IsNone then
                    let t = float k * dt

                    match GraphEngine.evalOnce g with
                    | EvalResult.Ok values ->
                        for id in distinctOutputs do
                            let v = if values.ContainsKey id then values[id] else nan
                            buffers[id].Add({ t = t; value = v })

                    | EvalResult.Error e ->
                        fail <- Some (evalErrorToHttp e)

            match fail with
            | Some err -> err
            | None ->
                let series =
                    buffers
                    |> Map.toList
                    |> List.map (fun (id, xs) -> {| id = id; samples = List.ofSeq xs |})

                Results.Ok({| ok = true; series = series |})

    [<EntryPoint>]
    let main args =

        let builder = WebApplication.CreateBuilder(args)

        builder.Services.AddEndpointsApiExplorer() |> ignore
        builder.Services.AddSwaggerGen() |> ignore

        builder.Services.AddCors(fun o ->
            o.AddDefaultPolicy(fun p ->
                p.AllowAnyOrigin().AllowAnyHeader().AllowAnyMethod() |> ignore
            )
        ) |> ignore

        let app = builder.Build()

        app.UseCors() |> ignore

        if app.Environment.IsDevelopment() then
            app.UseSwagger() |> ignore
            app.UseSwaggerUI() |> ignore

        app.MapGet("/health", Func<string>(fun () -> "ok")) |> ignore

        app.MapPost(
            "/api/demo/ramp",
            Func<RampRequest, IResult>(fun req ->
                let errors = validateRamp req
                if not errors.IsEmpty then
                    Results.BadRequest({| ok = false; errors = errors |})
                else
                    let samples = Engine.demoIntegratorRamp req.dt req.steps
                    let out: SampleDto list =
                        samples |> List.map (fun s -> { t = s.t; value = s.value })
                    Results.Ok({| ok = true; samples = out |})
            )
        ) |> ignore

        // Save graph
        app.MapPost(
            "/api/ui/graphs",
            Func<UiGraphDto, IResult>(fun ui ->
                let id = Guid.NewGuid().ToString("N")
                let now = DateTime.UtcNow
                let stored =
                    { meta = { id = id; createdAtUtc = now; updatedAtUtc = now }
                      graph = ui }
                writeGraph stored
                Results.Ok({| ok = true; id = id |})
            )
        ) |> ignore

        // List graphs
        app.MapGet(
            "/api/ui/graphs",
            Func<IResult>(fun () ->
                Results.Ok({| ok = true; items = listGraphs() |})
            )
        ) |> ignore

        // Get graph
        app.MapGet(
            "/api/ui/graphs/{id}",
            Func<string, IResult>(fun id ->
                match tryReadGraph id with
                | None -> Results.NotFound({| ok = false; errors = [ "Graph not found." ] |})
                | Some g -> Results.Ok({| ok = true; graph = g |})
            )
        ) |> ignore

        // Validate graph
        app.MapPost(
            "/api/ui/graphs/validate",
            Func<UiGraphDto, IResult>(fun ui ->
                let coreGraph, parseErrors = uiToCoreGraph ui
                let v = Graph.validate coreGraph
                let allErrors = parseErrors @ v.errors

                if allErrors.IsEmpty then
                    Results.Ok({| ok = true |})
                else
                    Results.BadRequest({| ok = false; errors = allErrors |})
            )
        ) |> ignore

        // Eval once
        app.MapPost(
            "/api/ui/graphs/eval-once",
            Func<UiGraphDto, IResult>(fun ui ->
                let coreGraph, parseErrors = uiToCoreGraph ui
                if not parseErrors.IsEmpty then
                    Results.BadRequest({| ok = false; errors = parseErrors |})
                else
                    let v = Graph.validate coreGraph
                    if not v.ok then
                        Results.BadRequest({| ok = false; errors = v.errors |})
                    else
                        match GraphEngine.evalOnce coreGraph with
                        | EvalResult.Ok values ->
                            let out =
                                values
                                |> Map.toList
                                |> List.map (fun (id, value) -> {| id = id; value = value |})
                            Results.Ok({| ok = true; values = out |})
                        | EvalResult.Error e ->
                            evalErrorToHttp e
            )
        ) |> ignore

        // Run saved graph
        app.MapPost(
            "/api/ui/graphs/{id}/run",
            Func<string, RunSavedRequest, IResult>(fun id req ->
                let reqErrors = validateRun req.dt req.steps req.outputs
                if not reqErrors.IsEmpty then
                    Results.BadRequest({| ok = false; errors = reqErrors |})
                else
                    match tryReadGraph id with
                    | None ->
                        Results.NotFound({| ok = false; errors = [ "Graph not found." ] |})
                    | Some stored ->
                        let coreGraph, parseErrors = uiToCoreGraph stored.graph
                        if not parseErrors.IsEmpty then
                            Results.BadRequest({| ok = false; errors = parseErrors |})
                        else
                            let v = Graph.validate coreGraph
                            if not v.ok then
                                Results.BadRequest({| ok = false; errors = v.errors |})
                            else
                                runStatelessSeries coreGraph req.dt req.steps req.outputs
            )
        ) |> ignore
        
        app.MapGet("/debug/cwd", Func<string>(fun () -> Directory.GetCurrentDirectory())) |> ignore

        app.Run()
        0