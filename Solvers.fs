module AdventOfCode2024.Solvers

open System.Reflection

let getSolvers () =
    let assembly = Assembly.GetExecutingAssembly()

    let solverTypes =
        assembly.GetTypes()
        |> Array.filter (fun t -> t.Namespace = "AdventOfCode2024" && t.Name.StartsWith("Day"))

    solverTypes
    |> Array.collect (fun t ->
        t.GetMethods(BindingFlags.Public ||| BindingFlags.Static)
        |> Array.filter (fun m -> m.Name = "solve")
        |> Array.map (fun m -> t.Name, m))
    |> Map.ofArray
