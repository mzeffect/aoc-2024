module AdventOfCode2024.Day06a

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

let isOutside (m: char[,]) (pos: Pos) =
    let (x, y) = pos
    x < 0 || x >= Array2D.length1 m || y < 0 || y >= Array2D.length2 m

let identifyThing c =
    match c with
    | '#' -> Obstacle
    | '^' -> Guard
    | _ -> Nothing

let thingAtPos (m: char[,]) (pos: Pos) =
    let (x, y) = pos

    if isOutside m (x, y) then
        Wall
    else
        m.[x, y] |> identifyThing

let guardPosition (matrix: char[,]) =
    matrix
    |> Array2D.mapi (fun i j c -> (i, j, c))
    |> Seq.cast<(int * int * char)>
    |> Seq.find (fun (i, j, _c) -> (identifyThing matrix.[i, j]) = Guard)
    |> fun (i, j, _) -> (i, j)

let rec walk (m: char[,]) (orientation: Orientation) (currentPos: Pos) (routeSoFar: Pos list) =
    let nextPosition = move orientation currentPos

    let nextAction =
        match thingAtPos m nextPosition with
        | Obstacle -> Turn Right
        | Wall -> Stop
        | _ -> Move nextPosition

    match nextAction with
    | Move nextPosition -> walk m orientation nextPosition (nextPosition :: routeSoFar)
    | Turn dir -> walk m (turn dir orientation) currentPos routeSoFar
    | Stop -> List.rev routeSoFar

let solve (input: string) =
    let map = parseToMatrix input

    let startingPos = map |> guardPosition
    let route = walk map North startingPos [ startingPos ]

    route |> Set.ofList |> Set.count |> string
