namespace AdventOfCode2023

open Common.Types
open System
open System.Collections.Generic

module Day17 =
    type Way =
        |N of int
        |W of int
        |S of int
        |E of int

    let getNeighbours (input:int array array) (limitMin,limitMax) (way:Way) (x,y) cost=
        match way with
        |N t when t <  limitMin                  -> [(x - 1, y), N (t + 1)]
        |N t when t >= limitMin && t < limitMax  -> [(x - 1, y), N (t + 1); (x, y - 1), W(1); (x, y + 1), E(1)]
        |N _                                     -> [(x, y - 1), W(1); (x, y + 1), E(1)]
        |W t when t <  limitMin                  -> [(x, y - 1), W (t + 1)]
        |W t when t >= limitMin && t < limitMax  -> [(x - 1, y), N(1); (x + 1, y), S(1); (x, y - 1), W(t + 1)]
        |W _                                     -> [(x - 1, y), N(1); (x + 1, y), S(1)]
        |S t when t <  limitMin                  -> [(x + 1, y), S (t + 1)]
        |S t when t >= limitMin && t < limitMax  -> [(x + 1, y), S(t + 1); (x, y - 1), W(1); (x, y + 1), E(1)]
        |S _                                     -> [(x, y - 1), W(1); (x, y + 1), E(1)]
        |E t when t < limitMin                   -> [(x, y + 1), E (t + 1)]
        |E t when t >= limitMin && t < limitMax  -> [(x - 1, y), N(1); (x + 1, y), S(1); (x, y + 1), E(t + 1)]
        |E _                                     -> [(x - 1, y), N(1); (x + 1, y), S(1)]
        |> List.filter (fun ((a,b),_) -> a >= 0 && a < input.Length && b >= 0 && b < input[0].Length)
        |> List.filter (fun ((a,b),d) ->
            match d with
            |N 1 when a < limitMin - 1-> false 
            |W 1 when b < limitMin - 1 -> false
            |S 1 when (input.Length - a) < limitMin -> false
            |E 1 when (input[0].Length - b) < limitMin -> false
            |_ -> true)
        |> List.map (fun ((a,b),way) -> ((a,b),input[a][b] + cost, way))

    let findPath (xs:int, ys:int) (xd:int, yd:int) limits (input:int array array) =
        let pq = PriorityQueue<((int*int)*int*Way),int>()
        pq.Enqueue(((xs,ys),0, E(0)),0)
        pq.Enqueue(((xs,ys),0, S(0)),0)

        let rec move (visited:((int*int)*Way) Set) = 
            let ((currentX,currentY), cost, way) = pq.Dequeue()
            if ((currentX, currentY) = (xd,yd))
            then cost
            else
            let possibleNexts = (getNeighbours input limits way (currentX,currentY) cost)
            possibleNexts 
            |> List.fold (fun (s:((int*int)*Way) Set) (c,p,w) -> 
                if (s.Contains(c,w)|>not)
                then 
                    (pq.Enqueue((c,p, w), p))
                    s.Add(c,w) 
                else s) visited 
            |> move
        move (Set.empty.Add((xs,ys),E(0)).Add((xs,ys),S(0)))

    let solve limits (input:string array) =
        input 
        |> Array.map(fun l -> l |> Seq.map (fun a -> a|>string|>int) |> Array.ofSeq)
        |> findPath (0,0) (input.Length - 1, input[0].Length - 1) limits
    
    let puzzle1 (input:string array) = 
         input |> solve (1,3)

    let puzzle2 (input:string array) =
        input |> solve (4, 10)
    
    let Solution = (new Solution(17, puzzle1, puzzle2) :> ISolution).Execute