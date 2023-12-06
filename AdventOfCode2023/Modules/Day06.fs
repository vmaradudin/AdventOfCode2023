namespace AdventOfCode2023

open Common.Types

module Day06 =

    let parseInput (input:string array)  =
        let time = input[0].Split(" ", System.StringSplitOptions.RemoveEmptyEntries) |> Array.tail |> Array.map int64
        let distance = input[1].Split(" ", System.StringSplitOptions.RemoveEmptyEntries) |> Array.tail|> Array.map int64
        Array.zip time distance

    let rec findChances a b (time, distance) =
        let newA =
            match a with
            |a when (a * (time - a) > distance) -> a
            |_ -> a + 1L
        
        let newB =
            match b with
            |b when (b * (time - b) > distance) -> b
            |_ -> b - 1L

        if (newA > newB) || ((a = newA) && (b = newB)) 
        then 
            newB - newA + 1L
        else
          findChances newA newB (time, distance)
        
    let puzzle1 input = 
        input 
        |> parseInput
        |> Array.map (fun t -> findChances 0 (t|>fst) t)
        |> Array.fold (*) 1L

    let puzzle2 (input:string array) = 
        input |> Array.map _.Replace(" ", "").Replace(":"," ")
        |> parseInput
        |> Array.map (fun t -> findChances 0 (t|>fst) t)
        |> Array.fold (*) 1L

    let Solution = (new Solution(6, puzzle1, puzzle2) :> ISolution).Execute
