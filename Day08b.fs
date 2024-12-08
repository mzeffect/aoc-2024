module AdventOfCode2024.Day08b

open AdventOfCode2024.Utils
open AdventOfCode2024.Day08a

let rec generatePositionsInDirection gridDim pos dir acc =
    let nextPos = Vec.add pos dir

    if isOutsideGridDimensions gridDim nextPos then
        acc
    else
        generatePositionsInDirection gridDim nextPos dir (nextPos :: acc)

let getAntinodesWithResonance (gridDim: int * int) (p1: Pos) (p2: Pos) =
    let direction = Vec.subtract p2 p1
    let generatePositions = (generatePositionsInDirection gridDim)

    p1 :: p2 :: (generatePositions p1 direction [])
    @ (generatePositions p2 (Vec.scale -1 direction) [])

let solve (input: string) =
    let grid = parseInput input

    grid
    |> getAntennaPositionsByFrequency
    |> getAntinodesByFrequency (gridSize grid) getAntinodesWithResonance
    |> Seq.collect snd
    |> Seq.distinct
    |> Seq.length
    |> string
