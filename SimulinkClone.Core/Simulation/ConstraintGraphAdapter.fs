namespace SimulinkClone.Core.Simulation

open SimulinkClone.Core.Simulation.ConstraintSolver

module ConstraintGraphAdapter =

    // OVO SU PRIVREMENI DTO TIPOVI
    // Ako tvoj Graph.fs već ima slične tipove, ove obriši
    // i zamijeni ih stvarnim tipovima iz Graph.fs

    type GraphBlockType =
        | Subtraction
        | Multiplication

    type GraphPort =
        { Name: string
          Value: float option }

    type GraphBlock =
        { Id: string
          BlockType: GraphBlockType
          Ports: GraphPort list }

    type GraphConnection =
        { FromBlockId: string
          FromPort: string
          ToBlockId: string
          ToPort: string }

    type GraphModel =
        { Blocks: GraphBlock list
          Connections: GraphConnection list }

    type SolvedGraphPort =
        { Name: string
          Value: float option }

    type SolvedGraphBlock =
        { Id: string
          Ports: SolvedGraphPort list
          Status: string }

    type SolvedGraph =
        { Blocks: SolvedGraphBlock list }

    let private toConstraintValue (value: float option) =
        match value with
        | Some v -> Known v
        | None -> Unknown

    let private fromConstraintValue (value: Value) =
        match value with
        | Known v -> Some v
        | Unknown -> None

    let private statusToString status =
        match status with
        | Ok -> "OK"
        | Underdetermined -> "Underdetermined"
        | Updated -> "Updated"
        | Error -> "Error"

    let private toConstraintBlock (graphBlock: GraphBlock) : BlockInstance =
        let blockDefinition =
            match graphBlock.BlockType with
            | Subtraction -> subtractionBlock graphBlock.Id
            | Multiplication -> multiplicationBlock graphBlock.Id

        let values =
            graphBlock.Ports
            |> List.map (fun p -> p.Name, toConstraintValue p.Value)
            |> Map.ofList

        { Block = blockDefinition
          Values = values
          Status = Underdetermined }

    let private toConstraintConnection (connection: GraphConnection) : Connection =
        { From =
            { BlockId = connection.FromBlockId
              Port = connection.FromPort }
          To =
            { BlockId = connection.ToBlockId
              Port = connection.ToPort } }

    let private toSolvedGraphBlock (block: BlockInstance) : SolvedGraphBlock =
        let ports =
            block.Block.Ports
            |> List.map (fun portName ->
                let value =
                    block.Values
                    |> Map.tryFind portName
                    |> Option.defaultValue Unknown
                    |> fromConstraintValue

                { Name = portName
                  Value = value })

        { Id = block.Block.Id
          Ports = ports
          Status = statusToString block.Status }

    let solveGraph (graph: GraphModel) : SolvedGraph =
        let solverBlocks =
            graph.Blocks |> List.map toConstraintBlock

        let solverConnections =
            graph.Connections |> List.map toConstraintConnection

        let solvedBlocks =
            runSolver solverBlocks solverConnections

        { Blocks = solvedBlocks |> List.map toSolvedGraphBlock }