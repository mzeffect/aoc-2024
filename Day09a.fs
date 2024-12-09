module AdventOfCode2024.Day09a

open AdventOfCode2024.Utils

let getInputArray (input: string) =
    input |> splitLines |> Seq.take 1 |> Seq.map splitToIntArray |> Seq.item 0

type Block =
    | Gap
    | FileBlock of int

let naiveExpand (input: int array) : Block array =
    let mutable blocks = []
    let mutable currentFileId = 0

    for i in 0 .. input.Length - 1 do
        let value = input.[i]
        let isGap = i % 2 <> 0

        if isGap then
            blocks <- blocks @ List.init value (fun _ -> Gap)
        else
            blocks <- blocks @ List.init value (fun _ -> FileBlock currentFileId)
            currentFileId <- currentFileId + 1

    blocks |> List.toArray

let naiveDefrag (arr: Block[]) =
    let gaps =
        arr
        |> Array.mapi (fun i v -> if v = Gap then i else -1)
        |> Array.filter (fun i -> i >= 0)

    let mutable res = Array.copy arr
    let mutable j = Array.length arr - 1

    for i in gaps do
        while j >= 0 && j >= i && res.[j] = Gap do
            j <- j - 1

        if j >= 0 then
            res[i] <- res.[j]
            res[j] <- Gap

    res

let naiveChecksum (arr: Block[]) =
    arr
    |> Array.mapi (fun i v ->
        match v with
        | FileBlock id -> (id * i) |> uint64
        | _ -> 0UL)
    |> Array.sum

let solve (input: string) =
    getInputArray input |> naiveExpand |> naiveDefrag |> naiveChecksum |> string
