module AdventOfCode2024.Day06b

open AdventOfCode2024.Utils
open AdventOfCode2024.Day06a

let thingAtPosWithObstacle (m: char[,]) (additionalObstaclePos: Pos) (pos: Pos) =
    if pos = additionalObstaclePos then
        Obstacle
    else
        thingAtPos m pos

let rec walkHasInfiniteLoop
    (map: char[,])
    (additionalObstaclePos: Pos)
    (orientation: Orientation)
    (currentPos: Pos)
    (visited: Set<Pos * Orientation>)
    =
    if Set.contains (currentPos, orientation) visited then
        true
    else
        let nextPosition = move orientation currentPos

        let nextAction =
            match thingAtPosWithObstacle map additionalObstaclePos nextPosition with
            | Obstacle -> Turn Right
            | Wall -> Stop
            | _ -> Move nextPosition

        let visitedWithCurrent = Set.add (currentPos, orientation) visited

        match nextAction with
        | Move nextPosition -> walkHasInfiniteLoop map additionalObstaclePos orientation nextPosition visitedWithCurrent
        | Turn dir -> walkHasInfiniteLoop map additionalObstaclePos (turn dir orientation) currentPos visitedWithCurrent
        | Stop -> false

let emptyPositions matrix =
    matrix
    |> Array2D.mapi (fun i j c -> (i, j, c))
    |> Seq.cast<(int * int * char)>
    |> Seq.filter (fun (i, j, _c) -> thingAtPos matrix (i, j) = Nothing)
    |> Seq.map (fun (i, j, _c) -> (i, j))
    |> Set.ofSeq

let solve (input: string) =
    let map = parseToMatrix input

    let startingPos = map |> guardPosition
    let emptyPositions = emptyPositions map

    let originalRoute = walk map North startingPos [ startingPos ]

    let candidateObstaclePositions =
        Set.intersect emptyPositions (Set.ofList originalRoute) |> Set.toSeq
        
    candidateObstaclePositions
    |> Seq.filter (fun emptyPos -> walkHasInfiniteLoop map emptyPos North startingPos Set.empty)
    |> Seq.length
    |> string
