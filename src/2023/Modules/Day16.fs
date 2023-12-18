namespace AdventOfCode2023

open Common.Types
open System
open System.Collections.Generic

module Day16 =
    let next (v:((int*int)*char)) =
        let ((x,y),d) = v
        match d with
        |'l' -> (x, y - 1)
        |'r' -> (x, y + 1)
        |'u' -> (x - 1, y)
        |'d' -> (x + 1, y)
        |_ -> failwith "Something went wrong!"
        |> fun a -> (a,d)

    let mutable cache = Dictionary<((int*int)*char),(((int*int)*char) array)>()
    
    let rec move (beams: ((int*int)*char) array) (acc: ((int*int)*char) array) (input:string array) = 
        let beams = beams |> Array.except acc
        if beams |> Array.isEmpty
        then
            acc |> Array.distinctBy (fst) |> Array.length
        else
            let newAcc = acc |> Array.append beams
            let newBeams = 
                beams 
                |> Array.map (fun ((i,j),d) ->
                    match input[i][j], d, i, j with
                    | '.', d, x, y ->    [|next ((x,y), d )|]
                    | '/', 'r', x, y ->  [|next ((x,y),'u')|]
                    | '/', 'd', x, y ->  [|next ((x,y),'l')|] 
                    | '/', 'l', x, y ->  [|next ((x,y),'d')|]
                    | '/', 'u', x, y ->  [|next ((x,y),'r')|]
                    | '\\', 'r', x, y -> [|next ((x,y),'d')|]
                    | '\\', 'd', x, y -> [|next ((x,y),'r')|] 
                    | '\\', 'l', x, y -> [|next ((x,y),'u')|]
                    | '\\', 'u', x, y -> [|next ((x,y),'l')|]
                    | '-', d, x, y when d = 'l' || d ='r' -> [|next ((x,y), d)|]
                    | '|', d, x, y when d = 'u' || d ='d' -> [|next ((x,y), d)|]
                    | '-', _, x, y -> [|next ((x,y), 'l') ; next ((x,y), 'r')|]
                    | '|', _, x, y -> [|next ((x,y), 'u') ; next ((x,y), 'd')|]
                    |_ -> failwith "Something went wrong!")
                |> Array.collect id
                |> Array.filter (fun ((x,y),_) -> x >= 0 && x < input.Length && y >= 0 && y < input[0].Length)

            move newBeams newAcc input

    let puzzle1 input = 
        input |> move [|(0,0),'r'|] [||]

    let puzzle2 (input:string array) =
        [|
            [|for i in 0..input[0].Length - 1 -> ((0,i),'d')|]
            [|for i in 0..input[0].Length - 1 -> ((input[0].Length - 1, i),'u')|]
            [|for i in 0..input.Length - 1 -> ((i,0),'r')|]
            [|for i in 0..input.Length - 1 -> ((0,i),'l')|]
        |] 
        |> Array.collect id
        |> fun a -> a
        |> Array.map (fun i -> move [|i|] [||] input)
        |> Array.max
        
    
    let Solution = (new Solution(16, puzzle1, puzzle2) :> ISolution).Execute