module AdventOfCode2024.Day10b

open System.Collections.Generic
open AdventOfCode2024.Day10a
open AdventOfCode2024.Utils

// this is very similar to part 1, but instead of accumulating peak coordinates
// we can count how many times a peak is reached via the recursion
let countUniquePathsToPeaks (map: HikeSegment [,]) (startingPos: Pos) =
    let rec recur (currentPos: Pos) (visited: Dictionary<Pos, int>) =
        if visited.ContainsKey(currentPos) then
            visited.[currentPos] // Return cached result for this position
        else
            let (x, y) = currentPos
            let segment = map.[x, y]

            let result =
                match segment with
                | Peak _ -> 1
                | _ ->
                    validNextDirections map currentPos segment
                    |> (fun dirs ->
                        match dirs with
                        | [] -> 0
                        | _ ->
                            dirs
                            |> List.sumBy (fun dir -> recur (move dir currentPos) visited))

            visited.[currentPos] <- result
            result

    recur startingPos (Dictionary<Pos, int>())

let solve (input: string) =
    let inputMap = input |> parseToMatrixWith charToInt |> toHikeMap

    inputMap
    |> getTrailheadPositions
    |> Seq.fold (fun acc trailhead -> acc + countUniquePathsToPeaks inputMap trailhead) 0
    |> string
