namespace AdventOfCode2023

open Common.Types
open System.Text.RegularExpressions

module Day05 =

    let parser (result:((int64*int64*int64) array) array)  (line:string) =
        match line with
        | "" -> result
        | l when l.EndsWith(":") -> Array.append result [|Array.empty|]
        |_ -> 
            line.Split(" ") |> Array.map int64 |> 
            fun a -> result |> Array.last |> fun b -> ([| (a[0], a[1], a[2]) |] |> Array.append b) 
            |> fun a -> [|a|]|> Array.append (result |> fun a -> (a |> Array.take( (a |> Array.length) - 1)))
    
    let parseInput (input:string array) =
        let seeds = input |> Array.head |> _.Split(" ") |> Array.tail |> Array.map int64
        let mappings = input |> Array.tail |> Array.fold parser Array.empty
        seeds, mappings

    let parseInput2 (input:string array) =
        let seeds = 
            input |> Array.head |> _.Split(" ") |> Array.tail |> Array.map int64
            |> Array.chunkBySize 2 |> Array.map (fun a -> [|a[0]; a[0]+a[1]-1L|]) |> Array.concat
        let mappings = input |> Array.tail |> Array.fold parser Array.empty |> Array.map (fun a -> a |> Array.map (fun (x,y,z) -> ((y, y + z - 1L),x)))
        seeds, mappings

    let folder (result:int64 array) (v:(int64*int64*int64) array) =
        result 
        |> Array.map (fun a -> 
            v|> Array.tryFind (fun (d,s,r) -> s <= a && (s+r) > a ) |>
            fun i ->
            match i with
            |None -> a
            |Some(x,y,z) -> a - y + x) 
    
    let folder2 (result:int64 array) (v:((int64*int64)*int64) array) =
        let result2 = 
            result |> Array.chunkBySize 2 
            |> Array.map (
                fun a -> 
                    v |> Array.filter (fun ((s,e),d) -> s > a[0] && s < a[0])
                    |> Array.append  |> Array.append
                    )
            |> Array.concat

        result2
        |> Array.map (fun a -> 
            v|> Array.tryFind (fun ((s,e),d) -> s <= a && a <= e) |>
            fun i ->
            match i with
            |None -> a
            |Some((x,y),z) -> a - x + z) 

    let puzzle1 input = 
        input |> parseInput |> 
        fun (seed, mappings) -> mappings|> Array.fold folder seed
        |> Array.min

    let puzzle2 input = 
        input |> parseInput2 |> 
        fun (seed, mappings) -> mappings|> Array.fold folder2 seed
        |> Array.min

    let Solution = (new Solution(5, puzzle1, puzzle2) :> ISolution).Execute