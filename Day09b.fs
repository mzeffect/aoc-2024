module AdventOfCode2024.Day09b

open AdventOfCode2024.Day09a

type CompactBlock =
    | Gap of int * int
    | File of int * int * int

let expandToCompactBlocks (input: int[]) : CompactBlock[] =
    let mutable blocks: CompactBlock list = []
    let mutable currentFileId = 0
    let mutable currentLocation = 0

    for i in 0 .. input.Length - 1 do
        let value = input.[i]
        let isGap = i % 2 <> 0

        if isGap then
            blocks <- Gap(currentLocation, value) :: blocks
        else
            blocks <- File(currentLocation, value, currentFileId) :: blocks
            currentFileId <- currentFileId + 1

        currentLocation <- currentLocation + value

    blocks |> List.rev |> List.toArray

let betterDefrag (blocks: CompactBlock[]) =
    let mutable gaps =
        blocks
        |> Array.choose (function
            | Gap(loc, len) -> Some(loc, len)
            | _ -> None)

    let mutable files =
        blocks
        |> Array.choose (function
            | File(loc, len, id) -> Some(loc, len, id)
            | _ -> None)
        |> Array.rev

    let mutable filei = 0

    while filei < Array.length files do
        let (fileLocation, fileLength, fileId) = files.[filei]

        match
            gaps
            |> Array.tryFindIndex (fun (gapLocation, gapLen) -> gapLen >= fileLength && gapLocation < fileLocation)
        with
        | Some gapIndex ->
            let (gapLocation, gapLength) = gaps.[gapIndex]
            files.[filei] <- (fst gaps.[gapIndex], fileLength, fileId)
            gaps.[gapIndex] <- (gapLocation + fileLength, gapLength - fileLength)
            gaps <- Array.append gaps [| (fileLocation, fileLength) |]
        | None -> ()

        filei <- filei + 1

    let filteredFiles = files |> Array.filter (fun (_, len, _) -> len > 0)
    filteredFiles

let compactChecksum (blocks: (int * int * int)[]) =
    blocks
    |> Array.fold
        (fun acc (loc, len, id) ->
            let loc = uint64 loc
            let len = uint64 len
            let id = uint64 id
            acc + ([ loc .. loc + len - 1UL ] |> List.map (fun pos -> id * pos) |> List.sum))
        0UL

let solve (input: string) =
    input
    |> getInputArray
    |> expandToCompactBlocks
    |> betterDefrag
    |> compactChecksum
    |> string
