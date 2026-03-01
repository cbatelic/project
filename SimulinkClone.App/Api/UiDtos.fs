namespace SimulinkClone.App.Api
open System

type UiNodeDto =
    { id: string
      kind: string
      constant: float option
      x: float
      y: float }

type UiEdgeDto =
    { fromId: string
      toId: string
      toPort: int }

type UiGraphDto =
    { nodes: UiNodeDto list
      edges: UiEdgeDto list }
    
type SaveGraphResponse =
    { ok: bool
      id: string }

type RunSavedRequest =
    { dt: float
      steps: int
      outputs: string list }

type SampleDto =
    { t: float
      value: float }

type RunSeriesDto =
    { id: string
      samples: SampleDto list }

type RunResponse =
    { ok: bool
      series: RunSeriesDto list
      errors: string list option }