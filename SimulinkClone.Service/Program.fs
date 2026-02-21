namespace SimulinkClone.Service

open System
open System.IO
open System.Text.Json
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting

open SimulinkClone.Core.Simulation

// =======================
// DTOs
// =======================

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

type UiRunRequest =
    { dt: float
      steps: int
      outputs: string list
      graph: UiGraphDto }

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

    let private graphsDir =
        Path.Combine(Directory.GetCurrentDirectory(), "Data", "graphs")

    let private ensureStorage () =
        Directory.CreateDirectory(graphsDir) |> ignore

    let private graphPath id =
        Path.Combine(graphsDir, $"{id}.json")


    let private validateRamp req =
        [ if req.dt <= 0.0 then "dt must be > 0"
          if req.steps <= 0 then "steps must be >= 1"
          if req.steps > 10000 then "steps too large (max 10000)" ]

    let private validateRun dt steps outputs =
        [ if dt <= 0.0 then "dt must be > 0"
          if steps <= 0 then "steps must be >= 1"
          if steps > 20000 then "steps too large (max 20000)"
          if outputs |> List.isEmpty then "outputs required" ]

    let private parseKind s =
        match (if isNull s then "" else s).Trim().ToLowerInvariant() with
        | "constant" -> Some Constant
        | "add" -> Some Add
        | "integrator" -> Some Integrator
        | _ -> None

    let private uiToCoreGraph (dto: UiGraphDto) : SimulinkClone.Core.Simulation.Graph * string list =
        let errors = ResizeArray<string>()

        let nodes : SimulinkClone.Core.Simulation.Node list =
            dto.nodes
            |> List.choose (fun n ->
                match parseKind n.kind with
                | None ->
                    errors.Add($"Unknown node kind '{n.kind}' for node '{n.id}'.")
                    None
                | Some k ->
                    Some
                        { SimulinkClone.Core.Simulation.Node.id = n.id
                          kind = k
                          constant = n.constant })

        let edges : SimulinkClone.Core.Simulation.Edge list =
            dto.edges
            |> List.map (fun e ->
                { SimulinkClone.Core.Simulation.Edge.fromId = e.fromId
                  toId = e.toId
                  toPort = e.toPort })

        ({ SimulinkClone.Core.Simulation.Graph.nodes = nodes
           edges = edges },
         List.ofSeq errors)

    let private evalErrorToHttp err =
        match err with
        | GraphInvalid errs ->
            Results.BadRequest({| ok = false; errors = errs |})
        | MissingInput (nodeId, port) ->
            let p = match port with | In1 -> 1 | In2 -> 2
            Results.BadRequest({| ok = false; errors = [ $"Missing input for node '{nodeId}' port {p}" ] |})
        | UnsupportedBlock (nodeId, kind) ->
            Results.BadRequest({| ok = false; errors = [ $"Unsupported block '{kind}' at node '{nodeId}'" ] |})


    let private writeGraph stored =
        ensureStorage()
        let json = JsonSerializer.Serialize(stored, jsonOptions)
        File.WriteAllText(graphPath stored.meta.id, json)

    let private tryReadGraph id =
        ensureStorage()
        let path = graphPath id
        if File.Exists(path) then
            let json = File.ReadAllText(path)
            Some (JsonSerializer.Deserialize<StoredGraph>(json, jsonOptions))
        else None

    let private listGraphs () =
        ensureStorage()
        Directory.GetFiles(graphsDir, "*.json")
        |> Array.choose (fun file ->
            try
                let json = File.ReadAllText(file)
                let g = JsonSerializer.Deserialize<StoredGraph>(json, jsonOptions)
                Some {
                    id = g.meta.id
                    createdAtUtc = g.meta.createdAtUtc
                    updatedAtUtc = g.meta.updatedAtUtc
                    nodeCount = g.graph.nodes.Length
                    edgeCount = g.graph.edges.Length
                }
            with _ -> None)
        |> Array.toList


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

        // Save graph
        app.MapPost("/api/ui/graphs",
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
        app.MapGet("/api/ui/graphs",
            Func<IResult>(fun () ->
                Results.Ok({| ok = true; items = listGraphs() |})
            )
        ) |> ignore

        // Get graph
        app.MapGet("/api/ui/graphs/{id}",
            Func<string, IResult>(fun id ->
                match tryReadGraph id with
                | None -> Results.NotFound({| ok = false |})
                | Some g -> Results.Ok({| ok = true; graph = g |})
            )
        ) |> ignore

        app.Run()
        0