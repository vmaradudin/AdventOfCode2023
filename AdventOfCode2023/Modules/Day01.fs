namespace AdventOfCode2023

open Common.Types

module Day01 =
    
    let folder (result: int array) (value: string) =
        match value with 
        |"" -> Array.insertAt 0 0 result
        |v  -> Array.insertAt 0 ((result |> Array.head) + (v |> int)) (result |> Array.tail)

    let countCaloriesByElves input = 
        Array.fold folder ([0]|> Array.ofList) input 

    let puzzle1 input = 
        input |> countCaloriesByElves |> Array.max

    let puzzle2 input =
        input |> countCaloriesByElves |> Array.sortDescending |> Array.take 3 |> Array.sum

    let Solution = (new Solution(1, puzzle1, puzzle2) :> ISolution).Execute