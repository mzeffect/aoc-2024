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
