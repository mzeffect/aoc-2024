open AdventOfCode2024

let solvers =
    [ "01a", Day01a.solve; "01b", Day01b.solve; "02a", Day02a.solve; "02b", Day02b.solve ] |> Map.ofList

let dayArg = System.Environment.GetCommandLineArgs().[1]

let day = if dayArg.Length = 1 then "0" + dayArg else dayArg
let dayNumber = day.Substring(0, day.Length - 1)

// get solver function for day
let solve = solvers[day]

// list all files in data/[day]/examples folder
let exampleFiles =
    System.IO.Directory.GetFiles("data/" + dayNumber + "/examples")
    |> Array.filter (fun f -> (not (f.EndsWith "_out.txt")))
    |> Array.filter (fun f -> f.Contains day)

// run solver function on each example file
printfn "Testing with examples..."

exampleFiles
|> Array.iter (fun f ->
    let input = System.IO.File.ReadAllText(f)
    let output = solve input
    let expectedOutput = System.IO.File.ReadAllText(f.Replace(".txt", "_out.txt"))

    if output <> expectedOutput.TrimEnd() then
        printfn $"Example %s{f} failed"
        printfn $"Expected: %s{expectedOutput}"
        printfn $"Actual: %s{output}"
    else
        printfn $"Example %s{f} passed")

let input =
    try
        System.IO.File.ReadAllText("data/" + dayNumber + "/input.txt")
    with :? System.IO.FileNotFoundException ->
        printfn $"Input file for day %s{day} not found"
        ""

if input.Length = 0 then
    // no input file found, exit
    exit 1

printfn "Solution:"
printfn $"%s{solve input}"
