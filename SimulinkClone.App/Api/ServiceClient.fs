module SimulinkClone.App.Api.ServiceClient

open System
open System.Net.Http
open System.Text
open System.Text.Json
open System.Threading.Tasks


type ConstraintUiBlockDto =
    { id: string
      kind: string
      constantValue: float option
      x: float option
      y: float option }

type ConstraintUiWireDto =
    { fromBlockId: string
      fromTerminal: string
      toBlockId: string
      toTerminal: string }

type ConstraintUiKnownValueDto =
    { blockId: string
      terminal: string
      value: float }

type ConstraintUiGraphDto =
    { blocks: ConstraintUiBlockDto list
      wires: ConstraintUiWireDto list
      knownValues: ConstraintUiKnownValueDto list }

type ConstraintTerminalResultDto =
    { name: string
      value: float option }

type ConstraintBlockResultDto =
    { id: string
      status: string
      terminals: ConstraintTerminalResultDto list }

type ConstraintRunResponseDto =
    { ok: bool
      blocks: ConstraintBlockResultDto list }
    
type SaveConstraintGraphResponse =
    { ok: bool
      id: string }

type ConstraintGraphListItemDto =
    { id: string
      createdAtUtc: DateTime
      updatedAtUtc: DateTime
      blockCount: int
      wireCount: int
      knownValueCount: int }

type ConstraintGraphListResponse =
    { ok: bool
      items: ConstraintGraphListItemDto list }
    
type StoredConstraintGraphMeta =
    { id: string
      createdAtUtc: DateTime
      updatedAtUtc: DateTime }

type StoredConstraintGraphDto =
    { meta: StoredConstraintGraphMeta
      graph: ConstraintUiGraphDto }

type GetConstraintGraphResponse =
    { ok: bool
      graph: StoredConstraintGraphDto }

type ServiceClient(baseUrl: string) =

    let http =
        let h = new HttpClient()
        h.BaseAddress <- Uri(baseUrl.TrimEnd('/') + "/")
        h

    let jsonOptions =
        JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase)

    member _.GetHealthAsync() : Task<string> =
        async {
            let! s = http.GetStringAsync("health") |> Async.AwaitTask
            return s
        }
        |> Async.StartAsTask

    /// Use this when API returns JSON body and you want it deserialized into 'Res
    member _.PostJsonAsync<'Req, 'Res>(url: string, req: 'Req) : Task<'Res> =
        async {
            let json = JsonSerializer.Serialize(req, jsonOptions)
            use content = new StringContent(json, Encoding.UTF8, "application/json")

            use! resp = http.PostAsync(url.TrimStart('/'), content) |> Async.AwaitTask
            let! body = resp.Content.ReadAsStringAsync() |> Async.AwaitTask

            if not resp.IsSuccessStatusCode then
                return raise (Exception(sprintf "HTTP %d: %s" (int resp.StatusCode) body))

            // Deserialize result
            return JsonSerializer.Deserialize<'Res>(body, jsonOptions)
        }
        |> Async.StartAsTask

    /// Use this when API returns NO JSON body (or you don't care about it)
    member _.PostJsonUnitAsync<'Req>(url: string, req: 'Req) : Task<unit> =
        async {
            let json = JsonSerializer.Serialize(req, jsonOptions)
            use content = new StringContent(json, Encoding.UTF8, "application/json")

            use! resp = http.PostAsync(url.TrimStart('/'), content) |> Async.AwaitTask
            let! body = resp.Content.ReadAsStringAsync() |> Async.AwaitTask

            if not resp.IsSuccessStatusCode then
                return raise (Exception(sprintf "HTTP %d: %s" (int resp.StatusCode) body))

            return ()
        }
        |> Async.StartAsTask
        
    member _.GetJsonAsync<'T>(url: string) : Task<'T> =
        task {
            let fullUrl =
                if url.StartsWith("http", StringComparison.OrdinalIgnoreCase) then url
                else baseUrl.TrimEnd('/') + "/" + url.TrimStart('/')

            use req = new HttpRequestMessage(HttpMethod.Get, fullUrl)
            let! res = http.SendAsync(req)
            res.EnsureSuccessStatusCode() |> ignore
            let! body = res.Content.ReadAsStringAsync()
            return JsonSerializer.Deserialize<'T>(body, jsonOptions)
        }
        
    member this.RunConstraintAsync(graph: ConstraintUiGraphDto) =
        this.PostJsonAsync<ConstraintUiGraphDto, ConstraintRunResponseDto>(
            "api/constraint/run",
            graph
        )

    member this.SaveConstraintAsync(graph: ConstraintUiGraphDto) =
        this.PostJsonAsync<ConstraintUiGraphDto, SaveConstraintGraphResponse>(
            "api/constraint/graphs",
            graph
        )

    member this.ListConstraintAsync() =
        this.GetJsonAsync<ConstraintGraphListResponse>(
            "api/constraint/graphs"
        )

    member this.GetConstraintAsync(id: string) =
        this.GetJsonAsync<GetConstraintGraphResponse>(
            $"api/constraint/graphs/{id}"
        )

    member this.RunSavedConstraintAsync(id: string) =
        this.PostJsonAsync<obj, ConstraintRunResponseDto>(
            $"api/constraint/graphs/{id}/run",
            null
        )