module SimulinkClone.App.Api.RunDtos

type SaveGraphResponse =
    { ok: bool
      id: string }

type RunSavedRequest =
    { dt: float
      steps: int
      outputs: string list }

type Sample =
    { t: float
      value: float }

type Series =
    { id: string
      samples: Sample list }

type RunResponse =
    { ok: bool
      series: Series list
      errors: string list option }