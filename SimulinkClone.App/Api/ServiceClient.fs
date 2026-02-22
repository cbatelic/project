module SimulinkClone.App.Api.ServiceClient

open System
open System.Net.Http
open System.Text
open System.Text.Json
open System.Threading.Tasks

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