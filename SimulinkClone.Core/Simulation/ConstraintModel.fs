namespace SimulinkClone.Core.Simulation

type ConstraintBlockId = string
type TerminalName = string

type ConstraintBlockKind =
    | Constant
    | Add
    | Subtract
    | Multiply
    | Gain
    | Monitor

type ConstraintTerminal =
    { Name: TerminalName }

type ConstraintBlock =
    { Id: ConstraintBlockId
      Kind: ConstraintBlockKind
      ConstantValue: float option
      Terminals: ConstraintTerminal list }

type TerminalRef =
    { BlockId: ConstraintBlockId
      Terminal: TerminalName }

type ConstraintWire =
    { FromRef: TerminalRef
      ToRef: TerminalRef }

type KnownTerminalValue =
    { Ref: TerminalRef
      Value: float }

type ConstraintGraph =
    { Blocks: ConstraintBlock list
      Wires: ConstraintWire list
      KnownValues: KnownTerminalValue list }