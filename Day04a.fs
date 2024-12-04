module AdventOfCode2024.Day04a

open AdventOfCode2024.Utils

type ShiftDirection =
    | NoShift
    | ShiftedLeft
    | ShiftedRight

/// Returns true if the given character array spells "XMAS" in either direction.
let isXmas (chars: char[]) =
    (chars.[0] = 'X' && chars.[1] = 'M' && chars.[2] = 'A' && chars.[3] = 'S')
    || (chars.[0] = 'S' && chars.[1] = 'A' && chars.[2] = 'M' && chars.[3] = 'X')

/// Returns the number of times "XMAS" appears in the character grid (2D array)
/// in any non-diagonal direction.
/// Can transpose the grid to count vertically instead of horizontally.
let countXmasNonDiagonal transpose grid =
    let lines = if transpose then gridCols grid else gridRows grid

    lines
    |> Seq.fold
        (fun acc line ->
            let countInLine =
                line
                |> Seq.windowed 4
                |> Seq.fold (fun lineAcc window -> if isXmas window then lineAcc + 1 else lineAcc) 0

            acc + countInLine)
        0
    
/// Returns a new grid with rows shifted in such a way that diagonals become verticals.
/// Pads the grid with '.', a character we don't care about.
let shiftDiagonals shiftDirection (grid: char[,]) =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid
    let padding = '.'

    Array2D.init rows (rows + cols - 1) (fun r c ->
        let col = c - (if shiftDirection = ShiftedRight then r else (rows - 1 - r))

        if col >= 0 && col < cols then grid.[r, col] else padding)

let solve (input: string) =
    let grid = parseToGrid input

    let countXmasHorizontal = countXmasNonDiagonal false
    let countXmasVertical = countXmasNonDiagonal true

    let shiftLeft = shiftDiagonals ShiftedLeft
    let shiftRight = shiftDiagonals ShiftedRight

    let horizontalCount = countXmasHorizontal grid
    let verticalCount = countXmasVertical grid
    let shiftedLeftVerticalCount = countXmasVertical (shiftLeft grid)
    let shiftedRightVerticalCount = countXmasVertical (shiftRight grid)

    horizontalCount
    + verticalCount
    + shiftedLeftVerticalCount
    + shiftedRightVerticalCount
    |> string
