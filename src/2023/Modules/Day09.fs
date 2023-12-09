namespace AdventOfCode2023

open Common.Types

module Day09 =
   
    let parseInput (input:string array)  =
        input |> Array.map (fun l -> l.Split(' ')|> Array.map int)

    let forward arr state  =
        (arr |> Array.last) + state
    let backward arr state  =
        (arr |> Array.head) - state

    let rec predict folder oasisReport =
        if oasisReport |> Array.last |> Array.forall ((=)0) 
        then 
            Array.foldBack folder oasisReport 0
        else
            oasisReport|> Array.last 
            |> Array.pairwise |> Array.map (fun (a,b) -> b - a)
            |> fun a -> Array.append oasisReport [|a|] 
            |> predict folder
    
        
    let puzzle1 input = 
        input |> parseInput
        |>Array.map (fun a -> [|a|] |> predict forward) |> Array.sum

    let puzzle2 (input:string array) = 
        input |> parseInput
        |>Array.map (fun a -> [|a|] |> predict backward) |> Array.sum
    
    let Solution = (new Solution(9, puzzle1, puzzle2) :> ISolution).Execute
