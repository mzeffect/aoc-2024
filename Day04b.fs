module AdventOfCode2024.Day04b

open AdventOfCode2024.Utils

let isMas = charsMatchStringUnidirectional "MAS"

let getXmasCoords grid shiftedGrid shiftDirection =
    let lines = byColumns shiftedGrid

    let originalRows = Array2D.length1 grid
    let originalCols = Array2D.length2 grid

    lines
    |> Seq.mapi (fun lineIndex line ->
        line
        |> Seq.windowed 3
        |> Seq.mapi (fun windowIndex chars ->
            if isMas chars then
                match shiftDirection with
                | NoShift -> Some(windowIndex + 1, lineIndex)
                | ShiftLeft ->
                    let row = windowIndex + 1
                    let col = lineIndex - (originalRows - 1 - row)

                    if col >= 0 && col < originalCols then
                        Some(row, col)
                    else
                        None
                | ShiftRight ->
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
    let grid = parseToMatrix input

    let shiftLeft = diagonalToVerticalShift ShiftLeft
    let shiftRight = diagonalToVerticalShift ShiftRight

    let leftShiftCoords = getXmasCoords grid (shiftLeft grid) ShiftLeft
    let rightShiftCoords = getXmasCoords grid (shiftRight grid) ShiftRight

    let allCoords = leftShiftCoords @ rightShiftCoords

    allCoords
    |> List.countBy id
    |> List.where (fun (_, v) -> v = 2) // if we have two of the same coordinate, that's an intersection
    |> List.length
    |> string
