namespace SimulinkClone.App.Api

open System
open System.Net.Http
open System.Net.Http.Json
open System.Threading.Tasks

type ConstraintUiBlockDto =
    { id: string
      kind: string
      constantValue: float option }

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

module ConstraintApi =

    let runConstraintGraphAsync (baseUrl: string) (graph: ConstraintUiGraphDto) =
        task {
            use client = new HttpClient()
            let url = $"{baseUrl.TrimEnd('/')}/api/constraint/run"

            let! response = client.PostAsJsonAsync(url, graph)
            response.EnsureSuccessStatusCode() |> ignore

            let! data = response.Content.ReadFromJsonAsync<ConstraintRunResponseDto>()
            return data
        }