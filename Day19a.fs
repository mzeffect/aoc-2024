module AdventOfCode2024.Day19a

open System
open AdventOfCode2024.Utils

let isDesignPossible (patterns: string []) (design: string) =
    let rec isPossible (design: string) =
        if design.Length = 0 then
            true
        else
            let uniqPatternLengths =
                patterns
                |> Seq.filter (fun p -> design.StartsWith(p))
                |> Seq.map (fun p -> p.Length)
                |> Seq.distinct

            if uniqPatternLengths |> Seq.isEmpty then
                false
            else
                uniqPatternLengths
                |> Seq.exists (fun patternLength -> design.Substring(patternLength) |> isPossible)

    isPossible design

let parseDay19Input (input: string) =
    let lines = input |> splitLines
    // available patterns
    let patterns =
        lines[0]
            .Split([| ',' |], StringSplitOptions.TrimEntries)
        |> Array.sortByDescending (fun s -> s.Length)

    let designs = lines[1..]

    patterns, designs

let solve (input: string) =
    let (patterns, designs) = parseDay19Input input

    designs
    |> Seq.countBy (isDesignPossible patterns)
    |> Seq.choose (fun (isPossible, count) -> if isPossible then Some count else None)
    |> Seq.head
    |> string
