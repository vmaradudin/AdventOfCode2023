namespace AdventOfCode2023

open Common.Types

module Day08 =
   
    let parseInput (input:string array)  =
        let directions = input[0] |> Seq.map (fun a -> match a with  |'R' -> snd |_ -> fst) |> Array.ofSeq
        let map = 
            input 
            |> Array.skip 2 
            |> Array.map (fun a -> a.Split([|"="; " " ; ","; "(" ;")"|], System.StringSplitOptions.RemoveEmptyEntries))
            |> Array.map (fun a -> (a[0], (a[1], a[2]))) 
            |> Map.ofArray
        (directions, map)

    let rec walk (directions:(string*string -> string) array) (map:Map<string, (string * string)>) (reachCondition: (string -> bool)) (steps:int64) (current:string) =
        if reachCondition(current)
        then 
            steps 
        else
        let direction = directions[(steps % (directions |> Array.length |> int64)) |> int]
        walk directions map reachCondition (steps + 1L) (map[current] |> direction)

    let rec gcd x y =
        match y with
        |0L -> x
        |_ -> gcd y (x % y)

    let lcm a b = a*b/(gcd a b)

    let rec lcmMultiple (numbers :int64 array) =
        if numbers.Length = 1 
        then 
            numbers[0]
        else
            let newV = lcm numbers[0] numbers[1]
            numbers |> Array.skip 2 |> Array.append [|newV|] |> lcmMultiple

    let toArray (map:Map<string,(string * string)>) :string array=
        [| for kvp in map -> kvp.Key |]

    let findStepsNumber (startCondition: string -> bool) (reachCondition: string -> bool) input =
        input
        |> parseInput
        |> fun (directions, map) ->  
            map |> toArray 
            |> Array.filter startCondition
            |> Array.map (walk directions map reachCondition 0L)
            |> lcmMultiple
        
    let puzzle1 input = 
        input |> findStepsNumber (fun t -> t = "AAA") (fun t -> t = "ZZZ")

    let puzzle2 (input:string array) = 
        input |> findStepsNumber (fun t -> t.EndsWith("A")) (fun t -> t.EndsWith("Z"))

    let Solution = (new Solution(8, puzzle1, puzzle2) :> ISolution).Execute
