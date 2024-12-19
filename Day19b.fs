module AdventOfCode2024.Day19b

open System.Collections.Generic
open AdventOfCode2024.Day19a

let isDesignPossibleWithPermutationCount (patterns: string []) (design: string) =
    let cache = Dictionary<string, bool * uint64>()

    let rec isPossible (design: string) (numberOfWaysToGetHere: uint64) =
        match cache.TryGetValue design with
        | true, (possible, waysToAchieve) -> possible, numberOfWaysToGetHere * waysToAchieve
        | false, _ ->
            let result =
                if design.Length = 0 then
                    true, numberOfWaysToGetHere
                else
                    let matchingPatterns =
                        patterns
                        |> Seq.filter (fun p -> design.StartsWith(p))
                        |> Seq.map (fun p -> p.Length)
                        |> Seq.countBy id
                        |> Seq.toList

                    if matchingPatterns.IsEmpty then
                        false, 0UL
                    else
                        let subResults =
                            matchingPatterns
                            |> List.map (fun (patternLength, count) ->
                                let remainingDesign = design.Substring(patternLength)
                                isPossible remainingDesign (numberOfWaysToGetHere * uint64 count))
                            |> List.filter fst

                        if subResults.IsEmpty then
                            false, 0UL
                        else
                            true, (subResults |> List.sumBy snd)

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
