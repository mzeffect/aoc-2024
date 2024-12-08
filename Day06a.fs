module AdventOfCode2024.Day06a

open System.Collections.Generic
open AdventOfCode2024.Utils

type Thing =
    | Nothing
    | Wall
    | Obstacle
    | Guard

type Action =
    | Move of Pos
    | Turn of Dir
    | Stop

let identifyThing c =
    match c with
    | '#' -> Obstacle
    | '^' -> Guard
    | _ -> Nothing

let isGuard c = (=) (identifyThing c) Guard

let thingAtPos (m: char[,]) (pos: Pos) =
    let (x, y) = pos

    if isOutsideGrid m (x, y) then
        Wall
    else
        m.[x, y] |> identifyThing

let walk (map: char[,]) (startingOrientation: Orientation) (startingPos: Pos) =
    let visited = HashSet<Pos>()

    let rec recur (currentPos: Pos) (orientation: Orientation) =
        let nextPosition = move orientation currentPos
        let nextThing = nextPosition |> thingAtPos map

        let nextAction =
            match nextThing with
            | Obstacle -> Turn Right
            | Wall -> Stop
            | _ -> Move nextPosition

        visited.Add(currentPos) |> ignore

        match nextAction with
        | Move nextPosition -> recur nextPosition orientation
        | Turn dir -> recur currentPos (turn dir orientation)
        | Stop -> visited

    recur startingPos startingOrientation

let solve (input: string) =
    let map = parseToMatrix input

    let startingPos = map |> findOnePosition isGuard
    let startingOrientation = North

    let visitedPositions = walk map startingOrientation startingPos

    visitedPositions |> hashSetCount |> string
