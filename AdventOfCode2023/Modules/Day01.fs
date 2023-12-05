namespace AdventOfCode2023

open Common.Types

module Day01 =
    
    let getDigit s =
        s |> String.filter (fun a -> a >='0' && a <= '9') |> fun a -> $"{a[0]}{a[a.Length - 1]}" |> int
        
    let folder (result: int) (value: string) =
        value |> getDigit |> (+) result

    let folder2 result (value:string) =
        value
            .Replace("one","one1one")
            .Replace("two","two2two")
            .Replace("three", "three3three")
            .Replace("four","four4four")
            .Replace("five", "five5five")
            .Replace("six", "six6six")
            .Replace("seven", "seven7seven")
            .Replace("eight", "eight8eight")
            .Replace("nine", "nine9nine") 
            |> folder result


    let puzzle1 input = input |> Array.fold folder 0

    let puzzle2 input = input |> Array.fold folder2 0

    let Solution = (new Solution(1, puzzle1, puzzle2) :> ISolution).Execute