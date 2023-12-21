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
        
    let puzzle1 (input:string array) = 
    
        let gardenPlots = input |> Array.mapi(fun x l -> l|> Seq.mapi(fun y s -> if s = '#' then None else Some(x,y))) |> Seq.collect id |> Seq.choose id |> Set.ofSeq
        let startingPoint = input|> Array.indexed |> Array.pick(fun (x, l) -> if l.IndexOf('S') < 0 then None else Some(x, l.IndexOf('S')))
        step gardenPlots set[startingPoint] 64
    
    let puzzle2 (input:string array) =
        0        

    let Solution = (new Solution(21, puzzle1, puzzle2) :> ISolution).Execute