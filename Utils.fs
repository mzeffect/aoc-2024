module AdventOfCode2024.Utils

open System

/// Split input string into lines. Removes empty lines.
let splitLines (input: string) =
    input.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)

/// Split input string into words. Removes empty entries.
let splitWords (input: string) =
    input.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
