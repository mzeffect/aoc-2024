module AdventOfCode2024.Day07b

open System
open AdventOfCode2024.Day07a

type Op =
    | Add
    | Multiply
    | Concat

type OpResult = uint64

let ops = [ Add; Multiply; Concat ]

let concatNums (a: uint64) (b: uint64) = (a |> string) + (b |> string) |> uint64

let applyOp =
    function
    | Add -> (+)
    | Multiply -> (*)
    | Concat -> concatNums

let rec generateAllOps nums : Op list seq =
    if Array.length nums <= 1 then
        Seq.singleton []
    else
        generateAllOps (Array.tail nums)
        |> Seq.collect (fun restOps -> ops |> List.map (fun op -> op :: restOps))

let applyOps nums ops maxValue : OpResult =
    let numsArray = nums |> Seq.toArray

    let rec apply (current: uint64 array) (ops: Op list) =
        match current.Length, ops with
        | _, []
        | 1, _ -> current.[0]
        | _, op :: restOps ->
            let result = (applyOp op) current.[0] current.[1]

            if result > maxValue then
                result
            else
                let newNums = Array.append [| result |] current.[2..]
                apply newNums restOps

    apply numsArray ops

let canProduceWithConcat (target: uint64, nums: uint64 seq) =
    let numsArray = nums |> Seq.toArray

    generateAllOps numsArray
    |> Seq.map (fun ops -> applyOps nums ops target)
    |> Seq.contains target

let arithmeticOnlyResults (tasks: TestCase seq) =
    let byArithmeticOnly =
        tasks
        |> Seq.groupBy (fun (target, nums) -> nums |> Seq.rev |> Day07a.canProduceValue target)

    let solved = byArithmeticOnly |> Seq.filter fst |> Seq.collect snd

    let unsolved =
        byArithmeticOnly |> Seq.filter (not << fst) |> Seq.collect snd |> Seq.toArray

    solved, unsolved

let solve (input: string) =
    let testCases = parseInput input

    let (solved, unsolved) = testCases |> arithmeticOnlyResults

    let solvedWithConcat =
        unsolved
        |> Array.splitInto (Environment.ProcessorCount / 2)
        |> Array.Parallel.map (Array.filter canProduceWithConcat)
        |> Array.concat

    Seq.append solved solvedWithConcat |> Seq.map fst |> Seq.sum |> string
