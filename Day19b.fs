module AdventOfCode2024.Day19b

open System.Collections.Generic
open AdventOfCode2024.Day19a

let isDesignPossibleWithPermutationCount (patterns: string []) (design: string) =
    let cache = Dictionary<string, bool * uint64>()

    let rec isPossible (design: string) (waysToGetHere: uint64) =
        match cache.TryGetValue design with
        | true, (possible, waysToAchieve) -> possible, waysToGetHere * waysToAchieve
        | false, _ ->
            let result =
                if design.Length = 0 then
                    true, waysToGetHere
                else
                    patterns
                    |> Seq.filter (fun p -> design.StartsWith(p))
                    |> Seq.map (fun p -> isPossible (design.Substring(p.Length)) (waysToGetHere * 1UL))
                    |> Seq.filter fst
                    |> Seq.fold (fun (_, total) (_, count) -> (true, total + count)) (false, 0UL)

            cache.Add(design, result)
            result

    isPossible design 1UL

let solve (input: string) =
    let (patterns, designs) = parseDay19Input input

    designs
    |> Seq.map (isDesignPossibleWithPermutationCount patterns)
    |> Seq.filter fst
    |> Seq.sumBy snd
    |> string
