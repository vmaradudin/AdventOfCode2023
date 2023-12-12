namespace AdventOfCode2023

open Common.Types
open System
open System.Collections.Generic

module Day12 =

    let parse (input:string array) =
        input 
        |> Array.map (fun l -> l.Split([|' '; ','|], StringSplitOptions.RemoveEmptyEntries) |> fun a -> a |> Array.head, (a |> Array.tail|> Array.map int))

    let mutable cache = Dictionary<string, int64>()

    let rec calculate (pattern:string , groups) =
        let patternT = pattern.Trim('.')
        let grStr = String.Join("-", (groups |> Array.map string))
        let cacheKey = $"{patternT}_{grStr}"

        if cache.ContainsKey(cacheKey) then cache.Item(cacheKey)
        else
        let result =
            match patternT, groups with
            | "", [||] -> 1L
            | "", _ -> 0L
            | s , [||] when not(s.Contains('#')) -> 1L
            | s , [||] -> 0L 
            | s , g when s.StartsWith('?')-> (calculate (s.Remove(0,1).Insert(0, "#"), groups)) + (calculate(s.Remove(0,1), groups))
            | s , g when 
                s.Length >= groups[0] && 
                s.Substring(0, groups[0]).Replace("#","").Replace("?","").Length = 0 && 
                (groups.Length = 1 || (s.Length > groups[0] && (s[groups[0]] = '?' || s[groups[0]] = '.'))) 
                -> calculate ((s.Substring(groups[0] + (match groups.Length with |1 -> 0 |_ -> 1))), (groups |> Array.tail))
            |_ -> 0L
        cache.Add(cacheKey, result)
        result
        
    let puzzle1 input = 
        input |> parse 
        |> Array.map calculate |> Array.sum

    let puzzle2 (input:string array) =
        input |> parse |> Array.map (fun (a,b)-> $"{a}?{a}?{a}?{a}?{a}", [|b;b;b;b;b|] |> Array.collect id)
        |> Array.map calculate |> Array.sum
    
    let Solution = (new Solution(12, puzzle1, puzzle2) :> ISolution).Execute