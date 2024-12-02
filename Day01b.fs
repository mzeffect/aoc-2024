module AdventOfCode2024.Day01b

open AdventOfCode2024.Day01a

let solve (input: string) =
    let (a, b) = inputToNumberArrays input
    let countsInA = Array.countBy id a
    let countsInB = Array.countBy id b |> Map.ofArray

    countsInA
    |> Array.map (fun (k, v) -> k * v * (Map.tryFind k countsInB |> Option.defaultValue 0))
    |> Array.sum
    |> string
