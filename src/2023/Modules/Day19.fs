namespace AdventOfCode2023

open Common.Types
open System.Text.RegularExpressions
open System

module Day19 =
    let partRegex = Regex("\{x=(?<x>\d+),m=(?<m>\d+),a=(?<a>\d+),s=(?<s>\d+)\}")
    let ruleRegex = Regex("(?<field>.*)(?<comparison>[<>])(?<argument>\d*):(?<action>.*)")

    type part = { x:int; m:int; a:int; s:int }
    type rule = { name:string; rules:(part -> bool) array}

    let parseRule (s:string)=
        match ruleRegex.Match(s) with
        |m when m.Success -> 
            let comparison =
                match m.Groups["comparison"].Value with
                |"<" -> (<)
                |">" -> (>)
                |_ -> failwith "Failed to parse rule comparison"
            let argument = m.Groups["argument"].Value |> int
            let breakerValue = if m.Groups["comparison"].Value = "<" then argument - 1 else argument
            let (func, breaker) =
                match m.Groups["field"].Value with
                |"x" -> (fun (p:part)-> p.x), {x = breakerValue; m = 0; a = 0; s = 0}
                |"m" -> (fun (p:part)-> p.m), {x = 0; m = breakerValue; a = 0; s = 0}
                |"a" -> (fun (p:part)-> p.a), {x = 0; m = 0; a = breakerValue; s = 0}
                |"s" -> (fun (p:part)-> p.s), {x = 0; m = 0; a = 0; s = breakerValue}
                |_ -> failwith "Failed to parse rule property"

            (fun (p:part) ->  
                if comparison (func(p)) argument 
                then Some(m.Groups["action"].Value) 
                else None)
                , breaker
        |_ -> (fun p -> Some(s)), { x = 0; m = 0; a = 0; s = 0}
        
    let parse (input:string array) =
        let rules =
            input 
            |> Array.takeWhile (fun l -> l <> "")
            |> Array.map (fun line -> 
                    line.Split([|'{';'}'; ','|], StringSplitOptions.RemoveEmptyEntries) 
                    |> fun s -> s[0], (s |> Array.tail |> Array.map(fun r -> parseRule(r))))
            |> dict
        let parts = 
            input|> Array.skipWhile(fun l -> l <> "") |> Array.tail
            |> Array.map 
                (fun textPart -> 
                    textPart
                    |> partRegex.Match 
                    |> (fun m -> 
                        { 
                            x = m.Groups["x"].Value|>int;
                            m = m.Groups["m"].Value|>int;
                            a = m.Groups["a"].Value|>int;
                            s = m.Groups["s"].Value|>int;
                        })
                )
        parts, rules

    let calculateRatingOfAcceptableParts (input:string array) =
        let (parts, rules) = input |> parse
        let rec applyRule (actions: (part -> string option) array) (part:part) =
            match (actions |> Array.head |>fun a ->  a(part)) with
            | Some(x) when x = "A" -> true
            | Some(x) when x = "R" -> false
            | Some(x) ->  applyRule (rules.Item x |> Array.unzip |> fst) part
            | _ -> applyRule (actions|>Array.tail) part
        parts 
        |> Array.filter (applyRule (rules.Item "in" |> Array.unzip |> fst))
        |> Array.sumBy (fun v -> v.x + v.m + v.a + v.s)

    let breakRange (breaker:part) (range:part*part) =
        let (r1,r2) =range
        match breaker.x, breaker.m, breaker.a, breaker.s with
        |x,0,0,0 when r1.x < x && r2.x > x -> [|(r1, {r2 with x = x});({r1 with x = x + 1}, r2)|]
        |0,m,0,0 when r1.m < m && r2.m > m -> [|(r1, {r2 with m = m});({r1 with m = m + 1}, r2)|]
        |0,0,a,0 when r1.a < a && r2.a > a -> [|(r1, {r2 with a = a});({r1 with a = a + 1}, r2)|]
        |0,0,0,s when r1.s < s && r2.s > s -> [|(r1, {r2 with s = s});({r1 with s = s + 1}, r2)|]
        |_ -> [|range|]

    let calculateValidCombinationsNumber (range) (input:string array) =
        let (_, rules) = input |> parse
            
        let rec applyRule (actions: ((part -> string option)*(part)) array) (range:part*part) =
            let (nextAction, breaker) = actions |> Array.head
            let ranges = breakRange breaker range
            if (ranges.Length > 1) 
            then
               ranges |> Array.collect (applyRule actions)
            else
            match (nextAction(ranges[0]|> fst)) with
            | Some(x) when x = "A" -> [|ranges[0]|]
            | Some(x) when x = "R" -> [||]
            | Some(x) ->  applyRule (rules.Item x) ranges[0]
            | _ -> applyRule (actions|>Array.tail) ranges[0]

        range 
        |> applyRule (rules.Item "in")
        |> Array.sumBy(fun (r1,r2) -> 
            [|
                (r2.a - r1.a + 1);
                (r2.x - r1.x + 1); 
                (r2.m - r1.m + 1);
                (r2.s - r1.s + 1)
            |] 
            |> Array.map int64 
            |> Array.fold (*) 1L )

    let puzzle1 (input:string array) = 
        input |> calculateRatingOfAcceptableParts
    
    let puzzle2 (input:string array) =
        input |> calculateValidCombinationsNumber ({x = 1; m = 1; a = 1; s = 1},{x = 4000; m = 4000; a = 4000; s = 4000})
                
    let Solution = (new Solution(19, puzzle1, puzzle2) :> ISolution).Execute