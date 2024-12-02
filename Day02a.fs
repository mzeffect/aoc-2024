module AdventOfCode2024.Day02a

open AdventOfCode2024.Utils

let isSafeStep (step: int * int) =
    let diff = abs (fst step - snd step)
    diff >= 1 && diff <= 3

let isAllIncreasing (steps: (int * int) array) =
    steps |> Seq.forall (fun (a, b) -> a < b)

let isAllDecreasing (steps: (int * int) array) =
    steps |> Seq.forall (fun (a, b) -> a > b)

let isMonotonic (steps: (int * int) array) =
    isAllIncreasing steps || isAllDecreasing steps

let isSafeReport (report: int array) =
    let steps = Array.pairwise report

    Array.forall isSafeStep steps && isMonotonic steps

let solve (input: string) =
    input
    |> splitLines
    |> Array.map splitWordsToIntArray
    |> Array.filter isSafeReport
    |> Array.length
    |> string
