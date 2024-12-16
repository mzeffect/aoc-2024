module AdventOfCode2024.Day11a

open AdventOfCode2024.Utils

let numDigits (n: uint64) = uint64 (floor (log10 (float n))) + 1UL

let isEven n = n % 2UL = 0UL

let split (stone: uint64) (numDigits: uint64) =
    let powerOf10 = numDigits / 2UL
    let divisor = pown 10UL (int powerOf10)
    [ stone / divisor; stone % divisor ]

let stonesAfterBlink (stone: uint64) =
    match stone with
    | 0UL -> [ 1UL ]
    | n ->
        match numDigits n with
        | digits when digits |> isEven -> (split n digits)
        | _ -> [ n * 2024UL ]

let blinkTimes (blinkCount: int) (stones: uint64 []) =
    let mutable stoneList = stones |> Array.toList

    for i in 1..blinkCount do
        stoneList <- stoneList |> List.collect stonesAfterBlink

    stoneList

let parseInput (input: string) =
    input |> splitWordsToIntArray |> Array.map uint64

let solve (input: string) =
    input
    |> parseInput
    |> blinkTimes 25
    |> List.length
    |> string
