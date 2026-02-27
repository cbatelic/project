namespace SimulinkClone.App.Api

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