open System.IO
open AdventOfCode2024.Solvers

let solvers = getSolvers ()

let dayArg = System.Environment.GetCommandLineArgs().[1]
let dayId = dayArg.Substring(0, dayArg.Length - 1)
let puzzleId = dayArg.Substring(3)

let solveMethod = solvers[dayArg]
let solve = fun input -> solveMethod.Invoke(null, [| input |]) :?> string

let exampleFiles =
    Directory.GetFiles("data/" + dayId + "/examples")
    |> Array.filter (fun f -> (not (f.EndsWith "_out.txt")))
    |> Array.filter (fun f -> f.Contains puzzleId)

printfn "Testing with examples..."

exampleFiles
|> Array.iter (fun f ->
    let input = File.ReadAllText(f)
    let output = solve input
    let expectedOutput = File.ReadAllText(f.Replace(".txt", "_out.txt"))

    if output <> expectedOutput.TrimEnd() then
        printfn $"❌  Example %s{f} failed"
        printfn $"Expected: %s{expectedOutput.TrimEnd()}"
        printfn $"Actual: %s{output}"
    else
        printfn $"✅  Example %s{f} passed")

let input =
    try
        File.ReadAllText("data/" + dayId + "/input.txt")
    with :? FileNotFoundException ->
        printfn $"Input file for %s{dayId} not found"
        ""

if input.Length = 0 then
    // no input file found, exit
    exit 1

printfn "Solution for puzzle input:"
printfn $"%s{solve input}"
