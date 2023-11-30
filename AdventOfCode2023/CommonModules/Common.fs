namespace Common

open System.IO

module Types =
    type ISolution =
        interface
            abstract member Execute : unit
    end

    let private execute day puzzle1 puzzle2 =
        (File.ReadAllLines (@"inputs\Input" + (day |> string|>fun a -> a.PadLeft(2,'0')) + ".txt"))
        |> fun i -> (day, (puzzle1 i |> string|> fun a -> a.PadLeft(15,' ')), (puzzle2 i |> string|> fun a -> a.PadLeft(15,' ')))
        |||> printfn "Day %2d | 1: %20s | 2: %20s"

    type Solution (executer)=
        interface ISolution with
            member x.Execute:unit = executer
                
        new (day:int, puzzle1: string[] -> int, puzzle2: string[] -> int) = new Solution(execute day puzzle1 puzzle2)
        new (day:int, puzzle1: string[] -> int64, puzzle2: string[] -> int64) = new Solution(execute day puzzle1 puzzle2)
        new (day:int, puzzle1: string[] -> int64, puzzle2: string[] -> int) = new Solution(execute day puzzle1 puzzle2)
        new (day:int, puzzle1: string[] -> int, puzzle2: string[] -> int64) = new Solution(execute day puzzle1 puzzle2)
        new (day:int, puzzle1: string[] -> string, puzzle2: string[] -> string) = new Solution(execute day puzzle1 puzzle2)
        new (day:int, puzzle1: string[] -> int, puzzle2: string[] -> string) = new Solution(execute day puzzle1 puzzle2)
        new (day:int, puzzle1: string[] -> string, puzzle2: string[] -> int) = new Solution(execute day puzzle1 puzzle2)
        
module Tools =
    let flatten array = 
        seq {for x in 0..((Array2D.length1 array)-1) -> array[x,*]}
        |> Seq.collect id |> Array.ofSeq