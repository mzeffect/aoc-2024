module AdventOfCode2024.Day04b

open AdventOfCode2024.Utils
open AdventOfCode2024.Day04a

/// Returns true if the given character array spells "MAS" in either direction.
let isMAS (chars: char[]) =
    (chars.[0] = 'M' && chars.[1] = 'A' && chars.[2] = 'S')
    || (chars.[0] = 'S' && chars.[1] = 'A' && chars.[2] = 'M')

/// Returns the coordinates of all "MAS" midpoints (the A letters) in a shifted character grid (2D array)
/// in any non-diagonal direction.
let getXmasCoords grid shiftedGrid shiftDirection =
    // transpose first, we are interested in the vertical direction only
    let lines = gridCols shiftedGrid

    // need the original grid dimensions so we can map the shifted/transposed coordinates back to the original
    let originalRows = Array2D.length1 grid
    let originalCols = Array2D.length2 grid

    lines
    |> Seq.mapi (fun lineIndex line ->
        line
        |> Seq.windowed 3
        |> Seq.mapi (fun windowIndex window ->
            if isMAS window then
                // we have a match, calculate the coordinate of "A" for the original grid
                match shiftDirection with
                | NoShift -> Some(windowIndex + 1, lineIndex)
                | ShiftedLeft ->
                    let row = windowIndex + 1
                    let col = lineIndex - (originalRows - 1 - row)

                    if col >= 0 && col < originalCols then
                        Some(row, col)
                    else
                        None
                | ShiftedRight ->
                    let row = windowIndex + 1
                    let col = lineIndex - row

                    if col >= 0 && col < originalCols then
                        Some(row, col)
                    else
                        None
            else
                None)
        |> Seq.choose id)
    |> Seq.concat
    |> List.ofSeq

let solve (input: string) =
    let grid = parseToGrid input

    let shiftLeft = shiftDiagonals ShiftedLeft
    let shiftRight = shiftDiagonals ShiftedRight

    let shiftedLeftVerticalCoords = getXmasCoords grid (shiftLeft grid) ShiftedLeft
    let shiftedRightVerticalCoords = getXmasCoords grid (shiftRight grid) ShiftedRight

    let allCoords = shiftedLeftVerticalCoords @ shiftedRightVerticalCoords

    allCoords
    |> List.countBy id // count occurence of each coordinate
    |> List.where (fun (_, v) -> v = 2) // if we have two of the same coordinate, that's an "X"
    |> List.length
    |> string
