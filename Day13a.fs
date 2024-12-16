module AdventOfCode2024.Day13a

open System
open System.Collections.Generic
open AdventOfCode2024.Utils

// a type that is either finite with a value, or infinite
// might come in handy later down the line
// currently supports additon and `min` - `compare` support is limited to when at least one of the operands is finite
// the base type has to support addition and comparison
type Value<'a when 'a: comparison and 'a: (static member (+): 'a * 'a -> 'a)> =
    | Finite of 'a
    | Infinite

    static member inline (+)(a: Value<'a>, b: Value<'a>) =
        match a, b with
        | Finite a, Finite b -> Finite(a + b)
        | _ -> Infinite

    static member inline Min(a: Value<'a>, b: Value<'a>) =
        match a, b with
        | Finite a, Finite b ->
            if compare a b <= 0 then
                Finite a
            else
                Finite b
        | Infinite, x -> x
        | x, Infinite -> x

    static member inline Compare(a: Value<'a>, b: Value<'a>) =
        match a, b with
        | Finite a, Finite b -> compare a b
        | Finite _, Infinite -> -1
        | Infinite, Finite _ -> 1
        | Infinite, Infinite -> 0

type ClawMachine =
    { ButtonA: Vec * int
      ButtonB: Vec * int
      PrizeLoc: Pos }

type Cost = Value<int>

let minimumCostToGetPrize (m: ClawMachine) =
    let visited = HashSet<Pos>()

    let rec minCostToGetTo (pos: Pos) (costSoFar: Cost) =
        if visited.Contains(pos) then
            Infinite
        else if costSoFar = Infinite then
            Infinite
        else if fst pos < 0 || snd pos < 0 then
            Infinite
        else if pos = (0, 0) then
            costSoFar
        else
            let (x, y) = pos
            let ((aX, aY), aCost) = m.ButtonA
            let ((bX, bY), bCost) = m.ButtonB

            visited.Add(pos) |> ignore
            let costViaA = minCostToGetTo (x - aX, y - aY) (costSoFar + (Finite aCost))
            let costViaB = minCostToGetTo (x - bX, y - bY) (costSoFar + (Finite bCost))

            min costViaA costViaB

    minCostToGetTo m.PrizeLoc (Finite 0)

let parseButton (s: string) =
    s.Split([| "X+"; ", Y+" |], StringSplitOptions.None)
    |> Array.tail
    |> fun arr -> (int arr.[0], int arr.[1])

let parseLocation (s: string) =
    s.Split([| "X="; ", Y=" |], StringSplitOptions.None)
    |> Array.tail
    |> fun arr -> (int arr.[0], int arr.[1])

let parseToClawMachines (input: string) =
    input
    |> splitLines
    |> Seq.chunkBySize 3
    |> Seq.map (fun machineSpec ->
        { ClawMachine.ButtonA = (parseButton machineSpec[0], 3)
          ButtonB = (parseButton machineSpec[1], 1)
          PrizeLoc = parseLocation machineSpec[2] })
    |> Seq.toArray

let solve (input: string) =
    input
    |> parseToClawMachines
    |> Seq.map minimumCostToGetPrize
    |> Seq.filter ((<>) Infinite)
    |> Seq.fold (+) (Finite 0)
    |> string
