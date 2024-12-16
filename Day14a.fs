module AdventOfCode2024.Day14a

open AdventOfCode2024.Utils
open System

type Robot = { Position: Pos; Velocity: Vec }

type RoomSize = int * int

let moveWithTeleport (gridSize: RoomSize) (times: int) (fromPos: Pos) (velocity: Vec) =
    let x, y = fromPos
    let xv, yv = velocity
    let xSize, ySize = gridSize
    let newX = ((x + times * xv) % xSize + xSize) % xSize
    let newY = ((y + times * yv) % ySize + ySize) % ySize
    newX, newY

// returns whether a position `p` is in a region defined by `rFrom` and `rTo`
let isInRegion (rFrom: Pos) (rTo: Pos) (p: Pos) =
    let (rFromX, rFromY) = rFrom
    let (rToX, rToY) = rTo
    let (x, y) = p

    rFromX <= x
    && x <= rToX
    && rFromY <= y
    && y <= rToY

let getQuadrantBounds (gridSize: RoomSize) =
    let xSize, ySize = gridSize
    let midX = xSize / 2
    let midY = ySize / 2

    [
      // top left quad
      ((0, 0), (midX - 1, midY - 1))
      // top right quad
      ((midX + 1, 0), (xSize - 1, midY - 1))
      // bottom left quad
      ((0, midY + 1), (midX - 1, ySize - 1))
      // bottom right quad
      ((midX + 1, midY + 1), (xSize - 1, ySize - 1)) ]
    |> List.toArray

let parseRobot (s: string) =
    s
    |> fun (s: string) ->
        s.Split([| '='; ' '; ',' |], StringSplitOptions.None)
        |> fun (parts: string []) ->
            { Robot.Position = (int parts[2], int parts[1])
              Velocity = (int parts[5], int parts[4]) }

let parseToRobots (input: string) =
    input
    |> splitLines
    |> Seq.map (fun robotSpec -> parseRobot robotSpec)
    |> Seq.toArray

let solve (input: string) =
    let robots = parseToRobots input

    let roomSize =
        if robots.Length = 12 then
            (7, 11)
        else
            (103, 101)

    let moveInRoom100Times = moveWithTeleport roomSize 100

    let robotPositionsAfter100Moves =
        robots
        |> Seq.map (fun { Position = p; Velocity = v } -> moveInRoom100Times p v)
        |> Seq.toArray

    roomSize
    |> getQuadrantBounds
    |> Array.map (fun (qFrom, qTo) ->
        robotPositionsAfter100Moves
        |> Array.filter (fun (robotPos) -> isInRegion qFrom qTo robotPos)
        |> Array.length)
    |> Array.fold (*) 1
    |> string
