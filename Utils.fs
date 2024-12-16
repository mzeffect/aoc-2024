module AdventOfCode2024.Utils

open System
open System.Collections.Generic

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

let splitToIntArray (input: string) : int[] =
    input.ToCharArray()
    |> Array.map (fun c -> int c - int '0')

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

/// Returns a matrix from parsing input string where each character of each line is a value.
/// Assumes all lines have the same length.
let parseToMatrixWith (parseElement: char -> 'a) (input: string) =
    let lines = splitLines input
    let (width, height) = lines.[0].Length, lines.Length
    Array2D.init width height (fun x y -> lines.[x].[y] |> parseElement)

let charToInt (c: char) = (int c) - (int '0')
    
let gridSize (arr: 'a[,]) =
    arr |> Array2D.length1, arr |> Array2D.length2

/// Returns a transposed matrix.
let transpose (arr: 'a[,]) =
    let rows = Array2D.length1 arr
    let cols = Array2D.length2 arr
    Array2D.init cols rows (fun i j -> arr.[j, i])

let findFirst (value: 'a) (array2d: 'a [,]) =
    let rows = Array2D.length1 array2d
    let cols = Array2D.length2 array2d
    
    seq {
        for i in 0 .. rows - 1 do
            for j in 0 .. cols - 1 do
                if array2d.[i,j] = value then
                    yield Some(i, j)
    }
    |> Seq.tryHead
    |> Option.defaultValue None

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

type Pos = int * int

type Orientation =
    | North
    | South
    | East
    | West

type Dir =
    | Left
    | Right

let turn (dir: Dir) (orientation: Orientation) =
    match orientation, dir with
    | North, Left -> West
    | North, Right -> East
    | South, Left -> East
    | South, Right -> West
    | East, Left -> North
    | East, Right -> South
    | West, Left -> South
    | West, Right -> North

let move (orientation: Orientation) (currentPos: Pos) =
    let (x, y) = currentPos

    match orientation with
    | North -> (x - 1, y)
    | South -> (x + 1, y)
    | East -> (x, y + 1)
    | West -> (x, y - 1)
    
let isOutsideGrid (m: 'a[,]) (pos: Pos) =
    let (x, y) = pos
    x < 0 || x >= Array2D.length1 m || y < 0 || y >= Array2D.length2 m
    
let isOutsideGridDimensions (dim: int * int) (pos: Pos) =
    let (x, y) = pos
    x < 0 || x >= fst dim || y < 0 || y >= snd dim

let findPositions (predicate: 'a -> bool) matrix : Pos seq =
    matrix
    |> Array2D.mapi (fun i j v -> (i, j, v))
    |> Seq.cast<(int * int * 'a)>
    |> Seq.filter (fun (_, _, v) -> (predicate v))
    |> Seq.map (fun (i, j, _) -> (i, j))

let distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let findOnePosition predicate matrix =
    matrix |> findPositions predicate |> Seq.head

let hashSetCount (set: HashSet<'a>) = set.Count

type Vec = int * int

module Vec =
    let subtract (x1,y1) (x2,y2) : Vec = 
        (x2 - x1, y2 - y1)  // Vector from p1 to p2
        
    let add (x1,y1) (x2,y2) : Pos = 
        (x1 + x2, y1 + y2)
        
    let scale n (x,y) : Vec =
        (n*x, n*y)
        
    let manhattan (x,y) = 
        abs x + abs y
        
    let length (x,y) =
        sqrt(float(x * x + y * y))
        
    // Some common vectors
    let up = (0,-1)
    let down = (0,1)
    let left = (-1,0)
    let right = (1,0)

let logWith (f: string) (thing: 'a) =
    printfn $"%s{f}: %A{thing}"
    thing
