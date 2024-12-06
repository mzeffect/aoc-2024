open System
open System.Diagnostics
open System.IO
open System.Threading
open AdventOfCode2024.Solvers

let solvers = getSolvers ()

let args = System.Environment.GetCommandLineArgs()
let runArg = args.[1]

let runSolver (dayArg: string) =
    let dayId = dayArg.Substring(0, dayArg.Length - 1)
    let puzzleId = dayArg.Substring(3)

    let solveMethod = solvers[dayArg]
    let solve = fun input -> solveMethod.Invoke(null, [| input |]) :?> string

    let input =
        try
            File.ReadAllText("data/" + dayId + "/input.txt")
        with :? FileNotFoundException ->
            printfn $"Input file for %s{dayId} not found"
            ""

    if runArg <> "runAll" then
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



        if input.Length = 0 then
            // no input file found, exit
            exit 1

        printfn "Solution for puzzle input:"
        let sw = Stopwatch.StartNew()
        printfn $"%s{solve input}"
        sw.Stop()
        printfn $"Solver runtime <%s{dayArg}>: %f{sw.Elapsed.TotalMilliseconds}ms"
    else
        // force garbage collection before benchmarking
        GC.Collect()
        GC.WaitForPendingFinalizers()
        Thread.Sleep(100)
        let sw = Stopwatch.StartNew()
        for i = 1 to 10 do
            solve input |> ignore
        sw.Stop()
        printfn $"Average runtime <%s{dayArg}>: %f{sw.Elapsed.TotalMilliseconds / float 10}ms"

if runArg = "runAll" then
    for solver in solvers do
        runSolver solver.Key

else
    runSolver runArg
