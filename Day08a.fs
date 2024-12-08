module AdventOfCode2024.Day08a

open AdventOfCode2024.Utils

type Frequency = char

type Thing =
    | Antenna of Frequency
    | Nothing

type Grid = Thing[,]

type AntinodeGenerator = (int * int) -> Pos -> Pos -> Pos list

let getAntinodes (gridDim: int * int) (p1: Pos) (p2: Pos) =
    let direction = Vec.subtract p2 p1

    [ Vec.add p1 direction; Vec.add p2 (Vec.scale -1 direction) ]
    |> List.filter (not << isOutsideGridDimensions gridDim)

let getAntinodesByFrequency (gridDim: int * int) (generateAntinodes: AntinodeGenerator) (antennaPositionsByFrequency) =
    antennaPositionsByFrequency
    |> Array.filter (snd >> Seq.length >> (<=) 2)
    |> Array.map (fun (thing, antennaPositions) ->
        match thing with
        | Antenna f ->
            antennaPositions
            |> Seq.allPairs antennaPositions
            |> Seq.filter (fun (p1, p2) -> p1 < p2)
            |> Seq.collect (fun (p1, p2) -> generateAntinodes gridDim p1 p2)
            |> Seq.distinct
            |> fun validAntinodes -> (f, validAntinodes)
        | _ -> failwith "expected antenna")

let isAntenna (t: Thing) =
    match t with
    | Antenna _ -> true
    | _ -> false

let getAntennaPositionsByFrequency (grid: Grid) =
    grid
    |> findPositions isAntenna
    |> Seq.groupBy (fun (x, y) -> grid.[x, y])
    |> Seq.toArray

let parseInput (input: string) =
    input
    |> parseToMatrixWith (function
        | '.' -> Nothing
        | f -> Antenna f)

let solve (input: string) =
    let grid = parseInput input

    grid
    |> getAntennaPositionsByFrequency
    |> getAntinodesByFrequency (gridSize grid) getAntinodes
    |> Seq.collect snd
    |> Seq.distinct
    |> Seq.length
    |> string
