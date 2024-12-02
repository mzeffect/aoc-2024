module AdventOfCode2024.Day01a

open AdventOfCode2024.Utils

let inputToNumberArrays (input: string) =
    input
    |> splitLines
    |> Array.map (fun line ->
        let nums = splitWords line
        (int nums.[0], int nums.[1]))
    |> Array.unzip

let solve (input: string) =
    input
    |> inputToNumberArrays
    |> fun (a, b) -> Array.zip (Array.sort a) (Array.sort b)
    |> Array.map (fun (x, y) -> abs (x - y))
    |> Array.sum
    |> string
