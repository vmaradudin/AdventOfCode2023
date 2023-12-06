namespace Common

open System.IO

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverageAttribute>]
module Types =
    type ISolution =
        interface
            abstract member Execute : unit
    end
    
    let private format i (task:string array -> 'a) =
        task i |> string |> _.PadLeft(15, ' ')        

    let private execute day puzzle1 puzzle2 =
        let start = System.DateTime.Now
        (File.ReadAllLines ($@"Inputs/Input{ day |> string|> _.PadLeft(2,'0')}.txt"))
        |> fun i -> (day, (puzzle1 |> format i), (puzzle2 |> format i), (System.DateTime.Now - start).TotalMilliseconds)
        |> fun (d, p1, p2, t) ->  printfn "Day %2d | 1: %20s | 2: %20s | Time: %f ms" d p1 p2 t

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
