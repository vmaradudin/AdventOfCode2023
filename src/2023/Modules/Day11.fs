namespace AdventOfCode2023

open Common.Types

module Day11 =
    let getExpansions (universe:string array) =
        let lineExpansions = universe |> Array.indexed |> Array.filter( fun (i, v) ->  v.Contains "#" |> not) |> Array.map fst
        let columnExpansions = [|0..((universe[0]|> String.length) - 1)|] |> Array.filter (fun i -> universe |> Array.forall(fun line -> line[i] ='.'))
        (lineExpansions, columnExpansions)

    let getDistance expansions expander (x1,x2) =
        let minX = min x1 x2
        let maxX = max x1 x2
        ((maxX - minX) |> int64) + ((expansions |> Array.filter(fun x -> minX<x && x<maxX) |> Array.length |> int64) * (expander - 1L))

    let computeDistance expander input =
        let expansions = input |> getExpansions
        let galaxies = 
            input 
            |> Array.mapi( fun i l -> l |> Seq.indexed |> Seq.filter (fun (j, c) -> c='#') |> Seq.map (fun (j,_) -> (i,j))|> Array.ofSeq)
            |> Array.collect id
        let pairs = galaxies|> Array.mapi (fun i u1 -> [|u1|] |> Array.allPairs (galaxies |> Array.skip (i + 1))) |> Array.collect id
        
        pairs 
        |> Array.map (fun ((x1,y1),(x2,y2)) -> (getDistance (fst(expansions)) expander (x1,x2)) + (getDistance (snd(expansions)) expander (y1,y2)))|> Array.sum

    let puzzle1 input = 
        input |> computeDistance 2L

    let puzzle2 (input:string array) = 
        input |> computeDistance 1000000L
    
    let Solution = (new Solution(11, puzzle1, puzzle2) :> ISolution).Execute