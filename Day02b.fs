module AdventOfCode2024.Day02b

open AdventOfCode2024.Utils
open AdventOfCode2024.Day02a

let isSafeReportWithTolerance (report: int array) =
    isSafeReport report
    || seq { for i in 0 .. report.Length - 1 -> Array.removeAt i report }
       |> Seq.exists isSafeReport

let solve (input: string) =
    input
    |> splitLines
    |> Array.map splitWordsToIntArray
    |> Array.filter isSafeReportWithTolerance
    |> Array.length
    |> string
