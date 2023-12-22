namespace AdventOfCode2023

open Common.Types
open System.Collections.Generic
open System

module Day22 =
    type point = { x:int; y:int; z:int; }
    type brick = { s: point; e: point; }

    let intersects brick1 brick2 =
        (not (brick1.s.x > brick2.e.x || brick1.e.x < brick2.s.x)) && 
        (not (brick1.s.y > brick2.e.y || brick1.e.y < brick2.s.y))

    let landBrick (pile:brick list) (brick:brick) = 
        let level =
            pile 
            |> List.tryFind (intersects brick)
            |> function |None -> 1 |Some(b) -> b.e.z + 1

        {s = {brick.s with z = level}; e = {brick.e with z = level + brick.e.z - brick.s.z}} :: pile

    let settleBricks (input:string array) = 
        input 
        |> Array.map (fun b -> 
            b.Split([|"~";","|], StringSplitOptions.None)
            |> Array.map int
            |> fun a -> { s = {x = a[0]; y = a[1]; z = a[2]}; e = {x = a[3]; y = a[4]; z = a[5]}})
        |> Array.sortBy (fun a -> a.s.z)
        |> Array.fold landBrick []
        |> List.indexed

    let supports (layer : (int*brick) list) (brickNumber:int, brick:brick) =
        let brickNumbers =
            layer 
            |> List.choose(fun (n, b) ->  
                ((n <> brickNumber)&&
                 (b.s.z - brick.e.z = 1) &&
                 (intersects b brick))
                |> function 
                   |true -> Some(n) |_ -> None)
        brickNumber, brickNumbers

    let layeredBy (selector:brick -> point) (pile: (int*brick) list) =
        pile
        |> List.groupBy (fun (i, b) -> b|> selector |> _.z)

    let supportedBy (layer:(int*brick) list) (brick:brick) =
        layer
        |> List.filter(fun (id, br) -> intersects br brick)

    let countCritical (pile:(int*brick) list) =
        let layeredByBottom = pile |> layeredBy _.s |> List.sortByDescending fst
        let layeredByTop = pile|> layeredBy _.e |> List.append [(0 , [])]|> List.sortByDescending fst |> dict
        layeredByBottom
        |> List.sumBy(fun (l,bricks) -> 
            bricks
            |> List.choose (
                fun (id,b) -> 
                    (supportedBy (layeredByTop.Item(l - 1)) b 
                    |> List.map(fun (id,_) -> id)) 
                    |> List.tryExactlyOne) 
            |> List.distinct
            |> List.length)

    let countFailed pile =
        let layeredByBottom = pile |> layeredBy _.s |> List.sortByDescending fst |> dict
        let layeredByTop = pile|> layeredBy _.e |> List.append [(0 , [])]|> List.sortByDescending fst |> dict
        layeredByBottom.Keys
        |> Seq.fold (
            fun (s,v) ->
                let supporters = layeredByTop.Item(v - 1)
                let supported = layeredByBottom.Item(v)
                supp
            ) ()
      

    let puzzle1 (input:string array) = 
        let res =
            input
            |> settleBricks
            |> fun bricks -> bricks.Length - (countCritical bricks)
        res
        //let res = input |> settleBricks
        //let supports =
        //    res |> List.map (fun i -> supports res i)

        //let supportedBy = 
        //    supports 
        //        |> List.collect(fun (b,t) -> (t|> List.map (fun i -> i, b)))
        //        |> List.groupBy(fun (t,b) -> t)
        //        |> List.map(fun (t,l) -> t, l|> List.map snd)
        //let singleSupporters = supportedBy |> List.filter(fun (t,l) -> l.Length = 1) |> List.collect snd

        //res |> List.filter(fun (i,_) -> singleSupporters |> List.contains i |> not) |> List.length
        
    let puzzle2 (input:string array) =
        let res = input |> settleBricks
        let layer1 = res |> List.choose(fun (i, b) -> if b.s.z = 1 then Some (i) else None)
        let supports =
            res 
            |> List.map (fun i -> supports res i)
            |> List.append [- 1, layer1]

        let supportingDictionary = supports |> dict
        // brickId, brickIdPulledOut
        let failingNumbers = Dictionary<int,int>()

        let supportedBy = 
            supports 
                |> List.collect(fun (b,t) -> (t|> List.map (fun i -> i, b)))
                |> List.groupBy(fun (t,b) -> t)
                |> List.map(fun (t,l) -> t, l|> List.map snd)
        
        let rec fail (state:(int*int list) list) (failedItems:int list) =
            let newState = 
                state 
                |> List.filter (fun (i,l) -> failedItems |> List.contains i |> not)
                |> List.map (fun (i,l) -> i, l |> List.except failedItems)

            let newFailed = 
                newState 
                |> List.filter (fun (i,l) -> l = [])
                |> List.map fst
            if newFailed = []
            then 
                failedItems.Length - 1
            else
                fail newState (failedItems @ newFailed)

        [1..res.Length] |> List.sumBy(fun i -> fail supportedBy [i])

    let Solution = (new Solution(22, puzzle1, puzzle2) :> ISolution).Execute