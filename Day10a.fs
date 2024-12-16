module AdventOfCode2024.Day10a

open System.Collections.Generic
open AdventOfCode2024.Utils

type HikeSegment =
    | Start of int
    | Mid of int
    | Peak of int

let toHikeMap grid =
    grid
    |> Array2D.map (function
        | 0 -> Start 0
        | 9 -> Peak 9
        | height -> Mid height)


let validNextDirections (map: HikeSegment [,]) (currentPos: Pos) (currentSegment: HikeSegment) =
    let isValidNext (dir: Orientation) =
        let nextPos = move dir currentPos

        if isOutsideGrid map nextPos then
            false
        else
            let x, y = nextPos
            let nextSegment = map.[x, y]

            match currentSegment, nextSegment with
            | Start h, Mid h' -> h' = h + 1
            | Mid h, Mid h' -> h' = h + 1
            | Mid h, Peak h' -> h' = h + 1
            | _ -> false

    [ North; South; East; West ]
    |> List.filter isValidNext

let discoverReachablePeaks (map: HikeSegment [,]) (startingPos: Pos) =
    let rec recur (currentPos: Pos) (peaksReached: Pos list) (visited: HashSet<Pos>) =
        if visited.Contains(currentPos) then
            peaksReached
        else
            let (x, y) = currentPos
            let segment = map.[x, y]

            match segment with
            | Peak _ -> currentPos :: peaksReached
            | _ ->
                visited.Add(currentPos) |> ignore

                validNextDirections map currentPos segment
                |> List.fold (fun peaks dir -> recur (move dir currentPos) peaks visited) peaksReached

    recur startingPos [] (HashSet<Pos>())
    |> Set.ofList

let getTrailheadPositions (map: HikeSegment [,]) =
    map
    |> findPositions (function
        | Start _ -> true
        | _ -> false)

let solve (input: string) =
    let inputMap = input |> parseToMatrixWith charToInt |> toHikeMap

    inputMap
    |> getTrailheadPositions
    |> Seq.map (discoverReachablePeaks inputMap)
    |> Seq.map (Set.count)
    |> Seq.sum
    |> string
