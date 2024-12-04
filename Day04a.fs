module AdventOfCode2024.Day04a

open AdventOfCode2024.Utils

let isXmas = charsMatchStringUnidirectional "XMAS"

let countXmasNonDiagonal transpose grid =
    let lines = if transpose then byColumns grid else byRows grid

    lines
    |> Seq.fold
        (fun acc line ->
            let countInLine =
                line
                |> Seq.windowed 4
                |> Seq.fold (fun lineAcc window -> if isXmas window then lineAcc + 1 else lineAcc) 0

            acc + countInLine)
        0

let solve (input: string) =
    let grid = parseToMatrix input

    let countXmasHorizontal = countXmasNonDiagonal false
    let countXmasVertical = countXmasNonDiagonal true

    let horizontalCount = countXmasHorizontal grid
    let verticalCount = countXmasVertical grid
    let leftShiftedCount = countXmasVertical (diagonalToVerticalShift ShiftLeft grid)
    let rightShiftedCount = countXmasVertical (diagonalToVerticalShift ShiftRight grid)

    horizontalCount
    + verticalCount
    + leftShiftedCount
    + rightShiftedCount
    |> string
