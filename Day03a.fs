module AdventOfCode2024.Day03a

open System.Text.RegularExpressions

let mulRegex = Regex(@"mul\((\d+),(\d+)\)")

let sumOfMultiplications (input: string) =
    input
    |> mulRegex.Matches
    |> Seq.fold (fun acc m ->
        let x = m.Groups.[1].Value |> int
        let y = m.Groups.[2].Value |> int
        acc + (x * y)
    ) 0
    
let solve (input: string) =
    input
    |> sumOfMultiplications
    |> string
