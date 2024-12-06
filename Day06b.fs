module AdventOfCode2024.Day06b

open System.Collections.Generic
open AdventOfCode2024.Utils
open AdventOfCode2024.Day06a

let thingAtPosWithExtraObstacle (m: char[,]) (additionalObstaclePos: Pos) (pos: Pos) =
    if pos = additionalObstaclePos then
        Obstacle
    else
        thingAtPos m pos

let walkHasInfiniteLoop (map: char[,]) (startingOrientation: Orientation) (startingPos: Pos) (extraObstaclePos: Pos) =
    let visited = HashSet<Pos * Orientation>()

    let rec recur (currentPos: Pos) (orientation: Orientation) =
        if visited.Contains((currentPos, orientation)) then
            true
        else
            let nextPosition = move orientation currentPos
            let nextThing = nextPosition |> thingAtPosWithExtraObstacle map extraObstaclePos

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

let solve (input: string) =
    let map = parseToMatrix input

    let startingPos = map |> findOnePosition isGuard
    let startingOrientation = North
    let emptyPositions = map |> findPositions isEmpty |> HashSet

    let originalVisitedPositions = walk map startingOrientation startingPos

    let candidates = emptyPositions
    candidates.IntersectWith(originalVisitedPositions)
    
    let hasLoop = walkHasInfiniteLoop map startingOrientation startingPos

    candidates
    |> Seq.filter hasLoop
    |> Seq.length
    |> string
