namespace AdventOfCode2023

open Common.Types
open Common.Math
open System.Collections.Generic
open System

module Day20 =

    type Module =
        {
            name: string;
            prefix: char;
            state: bool;
            recievers: string list;
            senders: string list;
        }
        member this.Recieve (broadband:Dictionary<string,Module>) signal = 
            match this.prefix, signal with
            |'%', true  -> None
            |'%', false -> this.state |> not |> Some
            |'&', _ -> this.senders |> List.forall (fun s -> (broadband.Item s)|> _.state) |> not |> Some
            |_ -> signal |> Some
            |> fun f ->
                broadband.Item this.name <- {this with state = f |> function |Some(x) -> x |_-> this.state}
                match f with
                |Some(x) -> this.recievers |> List.map(fun r -> r, x)
                |None -> []

    let broadband = Dictionary<string,Module>()
    let listener = Dictionary<bool, int>()
    let buttonSignal = ("broadcaster", false)

    let parse (input:string array) =
        let networkMap = 
            input
            |> List.ofArray
            |> List.map (fun l -> l.Split([|" -> "; ", "|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray)

        let network =
            networkMap
            |> List.map (fun l ->
                let title::recievers = l
                let label = title.Trim([|'%';'&'|])
                let prefix = title[0] |> function |x when x = '%' ||x = '&' -> x |_-> ' '
                {
                    name = label;
                    prefix = prefix;
                    state = false;
                    recievers = recievers;
                    senders = networkMap |> List.choose(fun (h::t) -> if (t|> List.contains label) then (h.Trim([|'%';'&'|]) |> Some) else None);
                })
        broadband.Clear();
        network |> List.iter(fun m -> broadband.Add(m.name, m))
        listener.Clear();
        listener.Add(false, 0)
        listener.Add(true, 0)
        broadband
    
    let rec processSignals steps (signals:(string*bool) list) =
        if signals = []
        then steps
        else
        let signal::restOfSignals = signals
        let (moduleName, pulse) = signal
        listener.Item(pulse) <- listener.Item(pulse) + 1
        let output =
            if broadband.ContainsKey moduleName 
            then
                broadband.Item(moduleName).Recieve(broadband) pulse
            else
                []
        processSignals (steps + 1) (restOfSignals @ output)

    let getRequiredSenders moduleName =
        let senders = 
            [for entry in broadband -> if (entry.Value.recievers |> List.contains moduleName) then Some(entry.Key) else None]
            |> List.choose id
            |> List.map (fun s -> broadband.Item(s))
        if (senders|> List.exists (fun t -> t.prefix <> '&')) then failwith("Unexpected Input!")
        senders |> List.collect _.senders

    let rec calculateStepsNumber buttonHits (state:(string*(int option)*(int option)) list) =
        if state |> List.forall(fun (a,b,c) -> b.IsSome && c.IsSome)
        then 
            state|> List.map(fun (_,v1, v2) -> v2.Value - v1.Value |> int64) |> Array.ofList |> arrayLCM
        else
            processSignals 0 [buttonSignal] |> ignore
            state
            |> List.map (fun (mName, firstHit, secondHit) ->
                match broadband.Item(mName).state, firstHit, secondHit with
                | true, None, _ -> (mName, Some(buttonHits), None)
                | true, Some(x), None -> (mName, Some(x), Some(buttonHits))
                |_ -> mName, firstHit, secondHit)
            |> calculateStepsNumber (buttonHits + 1)
    
    let calculateWhenFalseRecievedBy moduleName =
        getRequiredSenders moduleName
        |> List.map (fun l -> l,None,None)
        |> calculateStepsNumber 1
        

    let puzzle1 (input:string array) = 
        parse(input) |> ignore
        seq{for i in 1..1000 -> [buttonSignal]}
        |> Seq.fold processSignals 0 |> ignore
        listener.Values|> List.ofSeq |> List.map int64 |> List.fold (*) 1L
    
    let puzzle2 (input:string array) =
        parse(input) |> ignore
        calculateWhenFalseRecievedBy "rx"
                
    let Solution = (new Solution(20, puzzle1, puzzle2) :> ISolution).Execute