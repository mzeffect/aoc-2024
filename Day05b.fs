module AdventOfCode2024.Day05b

open AdventOfCode2024.Utils
open AdventOfCode2024.Day05a

let reorderWithRules (rules: Map<int, int array>) (input: int array) =
    let mutable current = Array.toList input

    let isValidList = (isValidSequence rules) << List.toArray
    let mustComeAfter = isRuleMapHit rules

    while not (current |> isValidList) do
        for i in 0 .. List.length current - 1 do
            let x = List.item i current
            let beforeX = List.take i current
            let needsMoving = beforeX |> List.filter (fun y -> mustComeAfter y x)

            for nm in needsMoving do
                let j = List.findIndex ((=) nm) current
                current <- moveElement current j i

    List.toArray current

let solve (input: string) =
    let (rules, prints) = parseInput input
    let pageOrderRules = getRuleMap rules
    let invalidPrints = prints |> Array.filter (not << isValidSequence pageOrderRules)

    invalidPrints
    |> Array.map (reorderWithRules pageOrderRules)
    |> Array.map getMiddleElement
    |> Array.sum
    |> string
