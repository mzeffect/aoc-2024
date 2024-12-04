module AdventOfCode2024.Utils

open System

/// Split input string into lines. Removes empty lines.
let splitLines (input: string) =
    input.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)

/// Split input string into words. Removes empty entries.
let splitWords (input: string) =
    input.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

/// Split a single-line string into an integer array.
let splitWordsToIntArray (line: string) =
    line
    |> splitWords
    |> Array.map int

/// Returns whether a given value is in the given range (end exclusive).
let inRange n (min, max) = min <= n && n < max

/// Create a 2D array from an input string where each character of each line is a value.
/// Assumes all lines have the same length.
let parseToGrid (input: string) =
    let lines = splitLines input
    let (width, height) = lines.[0].Length, lines.Length
    Array2D.init width height (fun x y -> lines.[x].[y])

let transposeGrid (arr: 'a[,]) =
    let rows = Array2D.length1 arr
    let cols = Array2D.length2 arr
    Array2D.init cols rows (fun i j -> arr.[j,i])
    
let gridRows grid =
    Seq.init (Array2D.length1 grid) (fun i -> grid.[i, *])

let gridCols grid =
    Seq.init (Array2D.length2 grid) (fun j -> grid.[*, j])
