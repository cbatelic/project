namespace SimulinkClone.Core.Domain

type BlockId = System.Guid

type BlockType =
    | Constant of float
    | Add
    | Integrator of initial: float

type Block =
    { Id: BlockId
      BlockType: BlockType }

type Connection =
    { From: BlockId
      To: BlockId }

type Model =
    { Blocks: Block list
      Connections: Connection list }
