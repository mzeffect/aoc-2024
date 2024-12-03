module AdventOfCode2024.Day03b

open System
open System.Text.RegularExpressions

/// returns all positions in the input where a regex matches
let matchPositions (rx: Regex) (input: string) =
    input |> rx.Matches |> Seq.map _.Index |> Seq.toArray

/// returns all multiplications along with their position in the input
let mulsWithPosition (input: string) =
    input
    |> Day03a.mulRegex.Matches
    |> Seq.map (fun m ->
        let x = m.Groups.[1].Value |> int
        let y = m.Groups.[2].Value |> int
        m.Index, x, y)
    |> Seq.toArray

/// given "do" and "don't" positions, returns a list of "do" ranges
let doRanges (doPositions: int array) (dontPositions: int array) =
    // combine all positions, mark them as enabled/disabled
    let positions =
        Array.append
            (Array.map (fun p -> (p, true)) doPositions)
            (Array.map (fun p -> (p, false)) dontPositions)
        |> Array.sort

    // build index ranges where operation is enabled
    let rec buildRanges acc isEnabled start positions =
        match positions with
        | [] -> if isEnabled then (start, Int32.MaxValue) :: acc else acc
        | (pos, enable) :: rest ->
            if enable <> isEnabled then
                if enable then // starts a new range
                    buildRanges acc enable pos rest
                else // ends the current range
                    buildRanges ((start, pos) :: acc) enable pos rest
            else // continue range
                buildRanges acc isEnabled start rest

    positions
    |> Array.toList
    |> buildRanges [] true 0
    |> List.rev
    |> List.toArray


let doRegexp = Regex(@"do\(\)")
let dontRegexp = Regex(@"don\'t\(\)")

let solve (input: string) =
    let doPositions = matchPositions doRegexp input
    let dontPositions = matchPositions dontRegexp input
    let multiplications = mulsWithPosition input

    let validRanges = doRanges doPositions dontPositions

    multiplications
    |> Seq.filter (fun m ->
        let (pos, _, _) = m
        validRanges |> Seq.exists (fun (a, b) -> a <= pos && pos < b))
    |> Seq.fold (fun acc (_, x, y) -> acc + (x * y)) 0
    |> string
