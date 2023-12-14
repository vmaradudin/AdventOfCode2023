namespace AdventOfCode2023

open Common.Types
open System
open System.Collections.Generic

module Day13 =

    let parse (input:string array) =
        String.Join(Environment.NewLine, input).Split($"{Environment.NewLine}{Environment.NewLine}")
        |> Array.map _.Split(Environment.NewLine)

    let checkmirror (lineNum:int) (array:string array) =
        let pairs = [|for i in 0..lineNum -> (lineNum - i - 1, lineNum + i ) |] |> Array.filter (fun (a,b) -> a >= 0 && b<array.Length)
        pairs |> Array.forall (fun (a,b) -> array[a] = array[b])

    
    let checkmirror2 (columnNum:int) (array:string array) =
        let pairs = [|for i in 0..columnNum -> (columnNum - i - 1, columnNum + i ) |] |> Array.filter (fun (a,b) -> a >= 0 && b<array[0].Length)
        pairs |> Array.forall (fun (a,b) -> array|> Array.forall(fun v -> v[a]=v[b]))

    let getMirrors (oldR,oldC) (input:string array)  =
        let numLines = ((input |> Array.length))
        let numCols = (input[0].Length)
        let lines = [|1..numLines - 1|]
        let cols = [|1..numCols - 1|]
        let potentialLines = lines |> Array.filter (fun i -> input[i-1] = input[i]) |> Array.sortDescending
        let potentialColumns = cols |> Array.filter (fun i -> input|> Array.forall(fun v -> v[i-1] = v[i])) |> Array.sortDescending
        let line = potentialLines |> Array.tryFind (fun a -> a <> oldR && checkmirror a input) |> function |Some(x) -> x |_ -> 0
        let column = potentialColumns |> Array.tryFind (fun a -> a<> oldC && checkmirror2 a input)|> function |Some(x) -> x |_ -> 0
        line * 100 + column
        
    let switch i (line:string) =
        let newS = 
            match line[i] with
            |'.' -> "#"
            |_ -> "."
        line.Remove(i,1).Insert(i,newS)

    let getMirrors2 (input: string array) =
        let oldValue = getMirrors (-1,-1)  input
        let ov = ((oldValue / 100), (oldValue % 100))
        [|0..(input.Length * input[0].Length) |] 
        |> Array.map (fun i -> input|> Array.mapi (fun x a -> if x <> (((i |> float) / (input[0].Length|>float))|>floor|> int) then a else (switch (i % input[0].Length) a)))
        |> Array.map (getMirrors ov) |> Array.distinct 
        |> Array.max

    let puzzle1 input = 
        input |> parse |> Array.map (getMirrors (-1,-1)) |> Array.sum

    let puzzle2 (input:string array) =
        input |> parse |> Array.map getMirrors2 |> Array.sum
        
    
    let Solution = (new Solution(13, puzzle1, puzzle2) :> ISolution).Execute