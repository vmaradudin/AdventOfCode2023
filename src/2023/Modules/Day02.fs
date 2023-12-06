namespace AdventOfCode2023

open Common.Types
open System.Text.RegularExpressions

module Day02 =

    let getMax color gameLine=
        let regex = new Regex($"(?<entries>(?<count>\d+) {color})")
        gameLine 
        |> regex.Matches
        |> Seq.map (fun a -> a.Groups["count"].Value |> int)
        |> Seq.max

    let puzzle1 input = 
        input 
        |> Seq.sumBy (fun b -> match b with | a when (getMax "red" a <= 12 && getMax "green" a <= 13 && getMax "blue" a <=14) -> (Regex.Match(a, "Game (?<count>\d+)").Groups["count"].Value |> int) |_ -> 0)

    let puzzle2 input = 
        input
        |> Seq.sumBy (fun a -> getMax "red" a * getMax "green" a * getMax "blue" a)

    let Solution = (new Solution(2, puzzle1, puzzle2) :> ISolution).Execute