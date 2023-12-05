namespace AdventOfCode2023

open Common.Types

module Day05 =

    let parser result  line =
        match line with
        | "" -> result
        | l when l.EndsWith(":") -> Array.append result [|Array.empty|]
        |_ -> 
            line.Split(" ") |> Array.map int64 |> 
            fun a -> result |> Array.last |> fun b -> ([| (a[0], a[1], a[2]) |] |> Array.append b) 
            |> fun a -> [|a|]|> Array.append (result |> fun a -> (a |> Array.take( (a |> Array.length) - 1)))

    let parseInput (input:string array)  =
        let seeds = 
            input |> Array.head |> _.Split(" ") |> Array.tail |> Array.map int64
            |> Array.chunkBySize 2 |> Array.map (fun a -> (a[0], a[0]+a[1]-1L))
        let mappings = 
            input |> Array.tail |> Array.fold parser Array.empty 
            |> Array.map (fun a -> a |> Array.map (fun (x,y,z) -> ((y, y + z - 1L), x - y)) |> Array.sortBy (fun ((s,_),_) -> s))
        // (start, end) ((start, end) diff)
        seeds, mappings


    let splitRange mappings (rangeStart,rangeEnd) =
        mappings
        |> Array.collect (fun ((s1,e1),_) -> [|s1; e1+1L;|])
        |> Array.append [|rangeStart; rangeEnd + 1L|]
        |> Array.distinct
        |> Array.filter (fun a -> a >= rangeStart && a <= (rangeEnd + 1L))
        |> Array.sort
        |> Array.windowed 2
        |> Array.map (fun a -> (a[0], a[1]-1L))
        
    let map ranges mappings =
        ranges
        |> Array.collect (splitRange mappings)
        |> Array.map (fun (s,e) -> 
            mappings 
            |> Array.tryFind (fun ((ms,me),_) -> ms<=s && me>=e) 
            |> fun q -> 
               match q with
               |None -> (s,e)
               |Some((x,y),z) -> (s + z, e + z))

    let puzzle1 input = 
        input 
        |> parseInput 
        |> fun (seed, mappings) -> ((seed |> Array.collect (fun (s,e) -> [|(s,s); (e - s,e - s)|])), mappings)
        |> fun (seed, mappings) -> mappings|> Array.fold map seed
        |> Array.minBy fst |> fst

    let puzzle2 input = 
        input |> parseInput |> 
        fun (seed, mappings) -> mappings|> Array.fold map seed
        |> Array.minBy fst |> fst

    let Solution = (new Solution(5, puzzle1, puzzle2) :> ISolution).Execute
