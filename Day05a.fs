module AdventOfCode2024.Day05a

open AdventOfCode2024.Utils

let parseInput (input: string) =
    let (ruleSection, dataSection) =
        match splitByEmptyLines input with
        | [| a; b |] -> (a, b)
        | _ -> failwith "Invalid input"

    let rules =
        ruleSection
        |> splitLines
        |> Array.map (splitWordsToIntArrayBy '|')
        |> Array.map (function
            | [| a; b |] -> (a, b)
            | _ -> failwith "Invalid input")

    let prints = dataSection |> splitLines |> Array.map (splitWordsToIntArrayBy ',')

    rules, prints

/// Returns a map of number -> number array from an array of tuples, grouped by the first element
let getRuleMap (rules: (int * int) array) =
    rules
    |> Array.groupBy fst
    |> Array.map (fun (key, vals) -> key, vals |> Array.map snd)
    |> Map.ofArray

let isRuleMapHit ruleMap x y =
    match Map.tryFind y ruleMap with
    | Some constraints -> Array.contains x constraints
    | None -> false

let isValidSequence (ruleMap) (items: int array) =
    let isRuleBreaking = isRuleMapHit ruleMap

    items
    |> Array.mapi (fun i x ->
        let before = items.[0 .. i - 1]
        not (before |> Array.exists (fun y -> isRuleBreaking y x)))
    |> Array.forall id

let solve (input: string) =
    let (rules, prints) = parseInput input
    let pagesOrderRules = getRuleMap rules
    let validPrints = prints |> Array.filter (isValidSequence pagesOrderRules)

    validPrints |> Array.map getMiddleElement |> Array.sum |> string
