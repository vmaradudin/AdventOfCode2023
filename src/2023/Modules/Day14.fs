namespace AdventOfCode2023

open Common.Types
open System
open System.Collections.Generic

module Day14 =

    let rec calculate (result:int,multipliers: int array) (index:int, line:string) =
        let line = line |> Array.ofSeq
        (line, multipliers)
        ||> Array.map2 (fun l m ->
            match l,m with
            |'O', m -> (m, m - 1)
            |'#', _ -> (0, index - 1)
            |_ -> (0, m))
        |> Array.unzip
        |> fun (a,b) -> (a |> Array.sum |> ((+) result), b)

    let tiltChunk (chunk: string) =
        let length = chunk.Length
        chunk.Replace(".","").PadRight(length,'.')
    let tiltLine (line:string)=
        line.Split('#')|> Array.map tiltChunk |> String.concat "#"

    let mutable cache = Dictionary<_,int>()
    let a2d (input:string array) =
        Array2D.init input.Length input[0].Length (fun i j -> input[i][j])

    let rec tiltCycle n (input:char[,]) =
        if n = 0
        then
            input
        else
        [|0..(input|>Array2D.length2) - 1|] 
        |> Array.map( fun i -> input[*,i]|> Array.map string|> String.concat "" |> tiltLine |> Seq.rev |> Seq.map string |> String.concat "")
        |> fun i -> tiltCycle (n - 1) (i|> a2d)
    
    let prin (input:char[,]) =
        for i in 0..(input|>Array2D.length1) - 1 do
            let s =  input[i,*] |> Array.map string |> String.concat ""
            printfn "%s" s


    let rec tilt n input =
        if n = 0
        then input
        else
            let key = [|0..(input |> Array2D.length1) - 1|]|> Array.map (fun i -> input[i,*] |> Array.map string |> String.concat "") |> String.concat""
            if cache.ContainsKey(key)
            then 
                printfn "%d : %d - %d"  (n - (cache.Item key)) n (cache.Item key)
            else
                cache.Add(key,n)

            let r = tiltCycle 4 input

            tilt (n - 1) r
    
    
    let puzzle1 (input:string array) = 
        let multipliers = Array.create input[0].Length input.Length
        input |> Array.indexed |> Array.map(fun (i,v) -> input.Length - i, v)
        |> Array.fold calculate (0, multipliers) |> fst

    
    let puzzle2 (input:string array) =
        let init = input |> a2d |> tilt 9
        let init2 = init |> tilt 7
        let res = [|0..(init |> Array2D.length1) - 1|]|> Array.map (fun i -> init[i,*] |> Array.map string |> String.concat "")
        let res2 = [|0..(init2 |> Array2D.length1) - 1|]|> Array.map (fun i -> init2[i,*] |> Array.map string |> String.concat "")
        res |> puzzle1
        
    
    let Solution = (new Solution(14, puzzle1, puzzle2) :> ISolution).Execute