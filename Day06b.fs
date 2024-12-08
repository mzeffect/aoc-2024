module AdventOfCode2024.Day06b

open System
open System.Collections.Generic
open System.Collections.Immutable
open AdventOfCode2024.Utils
open AdventOfCode2024.Day06a

let thingAtPosWithExtraObstacle (m: char[,]) (additionalObstaclePos: Pos) (pos: Pos) =
    if pos = additionalObstaclePos then
        Obstacle
    else
        thingAtPos m pos

let walkHasInfiniteLoop
    (map: char[,])
    (startingObstaclePositions: HashSet<Pos>)
    (startingOrientation: Orientation)
    (startingPos: Pos)
    (extraObstaclePos: Pos)
    =
    let visited = HashSet<Pos * Orientation>()

    let rec recur (currentPos: Pos) (orientation: Orientation) =
        if visited.Contains((currentPos, orientation)) then
            true
        else
            let nextPosition = move orientation currentPos

            let nextThing =
                if startingObstaclePositions.Contains nextPosition then Obstacle
                else if extraObstaclePos = nextPosition then Obstacle
                else if nextPosition |> isOutsideGrid map then Wall
                else Nothing

            let nextAction =
                match nextThing with
                | Obstacle -> Turn Right
                | Wall -> Stop
                | _ -> Move nextPosition

            visited.Add((currentPos, orientation)) |> ignore

            match nextAction with
            | Move nextPosition -> recur nextPosition orientation
            | Turn dir -> recur currentPos (turn dir orientation)
            | Stop -> false

    recur startingPos startingOrientation

let isEmpty c = (=) (identifyThing c) Nothing
let isObstacle c = (=) (identifyThing c) Obstacle

let solve (input: string) =
    let map = parseToMatrix input

    let startingPos = map |> findOnePosition isGuard
    let startingOrientation = North

    let originalVisitedPositions = walk map startingOrientation startingPos
    
    let emptyPositions = map |> findPositions isEmpty |> HashSet
    let obstaclePositions =
        map |> findPositions isObstacle |> HashSet

    let candidates = emptyPositions
    candidates.IntersectWith(originalVisitedPositions)

    let hasLoop =
        walkHasInfiniteLoop map obstaclePositions startingOrientation startingPos

    candidates
    |> Seq.toArray
    |> Array.splitInto (Environment.ProcessorCount / 3 + 1)
    |> Array.Parallel.map (fun extraObstaclePositions -> extraObstaclePositions |> Array.filter hasLoop |> Array.length)
    |> Array.sum
    |> string
