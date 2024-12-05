module AdventOfCode2024.Utils

open System

/// Split input string into lines. Removes empty lines.
let splitLines (input: string) =
    input.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)

/// Split input string into words. Removes empty entries.
let splitWords (input: string) =
    input.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

let splitWordsBy (separator: char) (input: string) =
    input.Split([| separator |], StringSplitOptions.RemoveEmptyEntries)

/// Split a single-line string into an integer array.
let splitWordsToIntArray (line: string) = line |> splitWords |> Array.map int

let splitWordsToIntArrayBy (separator: char) (line: string) =
    line |> splitWordsBy separator |> Array.map int

let splitByEmptyLines (input: string) =
    input.Split([| "\n\n" |], StringSplitOptions.RemoveEmptyEntries)

/// Returns whether a given value is in the given range (end exclusive).
let inRange n (min, max) = min <= n && n < max

// Array operations

let getMiddleElement (arr: 'a[]) =
    let middleIndex = Array.length arr / 2
    arr.[middleIndex]

// List operations

/// Moves an element from one index to another in a list
/// There is probably a better way to do this...
let moveElement list fromIndex toIndex =
    let element = List.item fromIndex list
    let listWithoutElement = List.removeAt fromIndex list
    let (before, after) = List.splitAt toIndex listWithoutElement
    before @ [ element ] @ after

// Matrix operations

type ShiftDirection =
    | NoShift
    | ShiftLeft
    | ShiftRight

/// Returns a matrix from parsing input string where each character of each line is a value.
/// Assumes all lines have the same length.
let parseToMatrix (input: string) =
    let lines = splitLines input
    let (width, height) = lines.[0].Length, lines.Length
    Array2D.init width height (fun x y -> lines.[x].[y])

/// Returns a transposed matrix.
let transpose (arr: 'a[,]) =
    let rows = Array2D.length1 arr
    let cols = Array2D.length2 arr
    Array2D.init cols rows (fun i j -> arr.[j, i])

/// Returns an array of all rows of a matrix.
let byRows grid =
    Array.init (Array2D.length1 grid) (fun i -> grid.[i, *])

/// Returns an array of all columns of a matrix.
let byColumns grid =
    Array.init (Array2D.length2 grid) (fun j -> grid.[*, j])

/// Returns a new grid with rows shifted in such a way that diagonals become verticals.
/// Pads the grid with '.', a character we don't care about.
let diagonalToVerticalShift shiftDirection (grid: char[,]) =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid
    let padding = '.'

    Array2D.init rows (rows + cols - 1) (fun r c ->
        let col = c - (if shiftDirection = ShiftRight then r else (rows - 1 - r))

        if col >= 0 && col < cols then grid.[r, col] else padding)

/// Returns whether the given character array matches the given string.
let charsMatchString (str: string) (chars: char[]) =
    let patternArray = str.ToCharArray()
    chars = patternArray

/// Returns whether the given character array matches the given string in either direction.
let charsMatchStringUnidirectional (str: string) (chars: char[]) =
    let patternArray = str.ToCharArray()
    chars = patternArray || chars = Array.rev patternArray

