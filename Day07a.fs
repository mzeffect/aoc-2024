module AdventOfCode2024.Day07a

open AdventOfCode2024.Utils

type Num = uint64
type TestCase = Num * Num seq

let parseInput (input: string): TestCase array =
    input
    |> splitLines
    |> Array.map splitWords
    |> Array.map (fun arr ->
        let testValue = Array.get arr 0 |> (fun s -> s.Substring(0, s.Length - 1) |> uint64)
        let nums = arr[1..] |> Array.map uint64
        (testValue, nums))

let rec canProduceValue value nums =
    if Seq.isEmpty nums then
        value = 0UL
    else
        let n, rest = Seq.head nums, Seq.tail nums

        if value % n = 0UL then
            canProduceValue (value / n) rest || canProduceValue (value - n) rest
        else
            canProduceValue (value - n) rest

let solve (input: string) =
    let tasks = parseInput input

    tasks
    |> Seq.filter (fun (testValue, nums) -> (Seq.rev nums) |> canProduceValue testValue )
    |> Seq.map fst
    |> Seq.sum
    |> string
