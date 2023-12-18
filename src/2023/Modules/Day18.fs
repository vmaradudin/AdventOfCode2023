namespace AdventOfCode2023

open Common.Types
open System.Text.RegularExpressions

module Day18 =

    let dec (hex:char) (acc:int64*int64) =
        let (result, m) = acc
        match hex with
        |s when s >= 'a' && s <= 'f' -> 10L + int64(s - 'a')
        |s ->  int64(s - '0')
        |> fun v -> 
            result + v * m,
            m * 16L
    
    let regex = Regex("(.) (\d*) \(#(.*)(\d)\)")
    
    let instructions1 (m: Match) = m.Groups[1].Value, int64(m.Groups[2].Value)
    let instructions2 (m: Match) = m.Groups[4].Value, (Seq.foldBack dec m.Groups[3].Value (0L, 1L))|> fst

    let parse (instr:Match -> string * int64) (input:string array) =
        input 
        |> Array.map (fun a -> a |> regex.Match |> instr)

    let calculateSquare (square: int64*(int64*int64)) (direction, steps) : (int64*(int64*int64))=
        let (sum,(x,y)) = square
        match direction with
        | "R" | "0" -> sum + steps * x,        (x, y + steps)
        | "L" | "2" -> sum - steps * (x - 1L), (x, y - steps)
        | "D" | "1" -> sum + steps,            (x - steps, y)
        | "U" | "3" -> sum,                    (x + steps, y)
        |_ -> sum, (x, y)

    let calculate steps =
        steps 
        |> Array.fold calculateSquare (1L,(0L, 0L))
        |> fst

    let puzzle1 (input:string array) = 
        input |> parse instructions1 |> calculate
    
    let puzzle2 (input:string array) =
        input |> parse instructions2 |> calculate
        
    let Solution = (new Solution(18, puzzle1, puzzle2) :> ISolution).Execute