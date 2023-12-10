namespace AdventOfCode2023

open Common.Types

module Day10 =
    let step (xp,yp) (x,y) input =
        match input[x][y] with
        |'|' -> [|(x - 1, y);(x + 1,y)|]
        |'-' -> [|(x, y - 1);(x,y + 1)|]
        |'L' -> [|(x - 1,y);(x,y + 1)|]
        |'J' -> [|(x, y - 1);(x - 1,y)|]
        |'7' -> [|(x,y - 1);(x+1,y)|]
        |'F' -> [|(x,y + 1);(x+1,y)|]
        |> Array.filter (fun a -> (xp,yp) <> a)
        |> Array.head


    let puzzle1 input = 
        input |> parseInput
        |>Array.map (fun a -> [|a|] |> predict forward) |> Array.sum

    let puzzle2 (input:string array) = 
        input |> parseInput
        |>Array.map (fun a -> [|a|] |> predict backward) |> Array.sum
    
    let Solution = (new Solution(10, puzzle1, puzzle2) :> ISolution).Execute
