module AdventOfCode2024.Day12a

open System.Collections.Generic
open AdventOfCode2024.Utils

type PlotType = Plot of char * int

let toPlotGrid (input: char [,]) =
    input |> Array2D.map (fun c -> Plot(c, 0))

let exploreRegions (plots: PlotType [,]) =
    let regionMask = Array2D.create (Array2D.length1 plots) (Array2D.length2 plots) 0
    let lookedAt = HashSet<Pos * Orientation>()
    let regionPrices = Dictionary<int, int>()
    let regionAreas = Dictionary<int, int>()
    let allDirs = [| North; South; East; West |]

    let canGoInDirection (fromPos: Pos) (region: int) (dir: Orientation) =
        let newPos = move dir fromPos

        match isOutsideGrid plots newPos with
        | true ->
            // contributes to the price of the region because the outside of the grid is also fenced
            regionPrices[region] <- regionPrices[region] + 1
            false
        | false ->
            let (fromX, fromY) = fromPos
            let (newX, newY) = newPos

            if regionMask.[newX, newY] = region then
                false
            else
                match plots.[fromX, fromY], plots.[newX, newY] with
                | Plot (a, _), Plot (b, _) when a = b -> true
                | _ ->
                    if not (lookedAt.Contains(fromPos, dir)) then
                        do
                            lookedAt.Add(fromPos, dir) |> ignore

                            // contributes to the price of the region because it's a region boundary, hence a fence
                            regionPrices[region] <- regionPrices[region] + 1

                    false

    let rec exploreRegion (fromPos: Pos) (currentDir: Orientation) (region: int) =
        let x, y = fromPos
        regionMask.[x, y] <- region

        // contributes to the area of the current region
        regionAreas[region] <- regionAreas[region] + 1

        let walkDirs =
            allDirs
            |> Seq.filter (canGoInDirection fromPos region)

        walkDirs
        |> Seq.iter (fun d -> (exploreRegion (move d fromPos) d region))
        |> ignore

    let tryGetNextStartingPos () = regionMask |> findFirst 0

    let mutable regionId = 0
    let mutable nextPos = Some((0, 0))

    while Option.isSome nextPos do
        match nextPos with
        | Some pos ->
            regionId <- regionId + 1
            regionPrices[regionId] <- 0
            regionAreas[regionId] <- 0

            exploreRegion pos East regionId

            nextPos <- tryGetNextStartingPos ()
        | None -> ()

    regionMask, regionPrices, regionAreas

let solve (input: string) =
    input
    |> parseToMatrix
    |> toPlotGrid
    |> exploreRegions
    |> fun (_, regionPrices, regionAreas) -> zipFoldDicts regionPrices regionAreas (fun price area -> price * area) 0
    |> string
