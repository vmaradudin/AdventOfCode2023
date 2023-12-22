namespace AdventOfCode2023

open Common.Types
open Common.Math
open System.Collections.Generic
open System

module Day21 =
    let rec step (gardenPlots:(int*int) Set) (positions:(int*int) Set) stepsLeft =
        if stepsLeft = 0
        then positions |> Set.count
        else
        positions
        |> Set.toSeq
        |> Seq.map(fun (x,y) -> set[(x+1,y);(x-1,y);(x,y+1);(x,y-1)])
        |> Set.unionMany
        |> Set.intersect gardenPlots
        |> fun np -> step gardenPlots np (stepsLeft - 1)
    
    let cache = Dictionary<(int*int*int*int*int), int>()

    let rec findCycle gardenPlots size positions stepsCount =
        let stepsCount = stepsCount + 1
        let newPositions =
            positions
            |> Set.toSeq
            |> Seq.map(fun (x,y) -> set[(x+1,y);(x-1,y);(x,y+1);(x,y-1)])
            |> Set.unionMany
            |> Set.filter(fun (x,y) -> gardenPlots |> Seq.contains ((x%size |> function |v when x< 0 -> v+size |v-> v ), (y%size |> function |v when v< 0 -> v+size |v -> v )))

        let center = newPositions |> Set.intersect gardenPlots |> Set.count
        let left =  newPositions |> Set.filter(fun (x,y) ->  (x >= 0 && x < size && y >= - size && y < 0         )) |> Set.count
        let right = newPositions |> Set.filter(fun (x,y) -> (x >= 0 && x < size && y >=  size  && y<  (2 * size))) |> Set.count
        let up =    newPositions |> Set.filter(fun (x,y) -> (x >= - size && x < 0        && y >=0 && y < size)) |> Set.count 
        let down =  newPositions |> Set.filter(fun (x,y) -> (x >= size   && x < 2 * size && y >=0 && y < size)) |> Set.count
        let key = (center,left, right, up, down)
        if cache.ContainsKey(key)
        then
            printfn $"Have Cache hit: prev: {cache.Item key}. Now: {stepsCount}"
            cache.Item key <- stepsCount
        else
            cache.Add(key, stepsCount)
            
        findCycle gardenPlots size newPositions (stepsCount)

    let puzzle1 (input:string array) = 
        let gardenPlots = input |> Array.mapi(fun x l -> l|> Seq.mapi(fun y s -> if s = '#' then None else Some(x,y))) |> Seq.collect id |> Seq.choose id |> Set.ofSeq
        let startingPoint = input|> Array.indexed |> Array.pick(fun (x, l) -> if l.IndexOf('S') < 0 then None else Some(x, l.IndexOf('S')))
        step gardenPlots set[startingPoint] 64
    
    let puzzle2 (input:string array) =
        let gardenPlots = input |> Array.mapi(fun x l -> l|> Seq.mapi(fun y s -> if s = '#' then None else Some(x,y))) |> Seq.collect id |> Seq.choose id |> Set.ofSeq
        let startingPoint = input|> Array.indexed |> Array.pick(fun (x, l) -> if l.IndexOf('S') < 0 then None else Some(x, l.IndexOf('S')))
        findCycle gardenPlots input.Length set[startingPoint] 0
        0

    let Solution = (new Solution(21, puzzle1, puzzle2) :> ISolution).Execute