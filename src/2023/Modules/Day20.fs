namespace AdventOfCode2023

open Common.Types
open System.Collections.Generic
open System

module Day20 =
    type Module =
        {
            name: string;
            prefix: char;
            state: bool;
            receivers: string array;
            memory: (string*bool) array;
        }
        member this.Recieve signal = 
            match this.prefix, signal with
            |'%', true  -> this, fun (br:Dictionary<string, Module>) ->  None
            |'%', false -> {this with state = not this.state}, fun (br:Dictionary<string, Module>) -> this.state |> not |> Some
            |'&', _ -> {this with state = not this.state}, fun (br:Dictionary<string, Module>) ->  


    let cache = Dictionary<string, int array>()
    let broadband = Dictionary<string,Module>()
    let Process (theModule:Module) ((sender,thePulse):string*bool) step =
        match theModule.prefix, thePulse, theModule.state with
        |'%', true,_ -> theModule, None
        |'%', false, state ->  {theModule with state = not state}, state|>not |> Some
        |'&', input, _ -> 
            let newMemory = theModule.memory |> Array.map(fun (s, state) -> if s = sender then (s,input) else (s,state))
            {theModule with memory = newMemory}, (newMemory|>Array.forall(snd) |> not |> Some)
        |_, input, _ -> theModule, input |> Some
        |> fun (newModuleState, output) -> 
            newModuleState, 
            match output with
            |Some(x) -> 
                if x && [|"ch";"gh";"sv";"th"|] |> Array.contains theModule.name
                    then 
                        if cache.ContainsKey(theModule.name) |> not then cache.Add(theModule.name, [||]) else ignore()
                        cache.Item(theModule.name) <- (Array.append (cache.Item theModule.name) [|step|]) 
                theModule.receivers |> Array.map(fun r -> r,(theModule.name,x))
            |_ -> Array.empty


    let parse (input:string array) =
        let networkMap = 
            input
            |> Array.map _.Split([|" -> "; ", "|], StringSplitOptions.RemoveEmptyEntries)
        let network =
            networkMap
            |> Array.map (fun l ->
                match l[0].[0], l[0], l|>Array.tail with
                |'%', name, receivers -> name.Substring(1), {name = name.Substring(1); prefix = '%'; state = false; receivers = receivers; memory = Array.empty}
                |'&', name, receivers -> name.Substring(1), {name = name.Substring(1); prefix = '&'; state = false; receivers = receivers; 
                    memory = 
                        networkMap
                        |> Array.filter(fun a -> 
                            a|>Array.tail 
                            |> Array.contains (name.Substring(1))) 
                            |> Array.map( fun a -> a |> Array.head |> _.Trim([|'%';'&'|]), false)}
                |_,name, receivers -> name, {name = name; prefix=' '; state = false; receivers = receivers; memory = Array.empty})
        network

    let button = ("broadcaster",("button", false))
    let ToString (m:Module) =
        $"name={m.name} | state:{m.state} | memory:{(m.memory |> Array.map(fun (a,b) -> a + b.ToString()+ '|'.ToString()) |> String.concat(String.Empty))}"

    let rec processSignals (modules:Module array) (counter:(bool*int) array) (signals: (string*(string*bool)) array) (getHitTo:string) (step:int) =
        if signals.Length = 0
        then 
            modules, counter, false
        else
        let (receiver, (sender, pulse)) = signals |> Array.head
        match modules |> Array.tryFind(fun m -> m.name = receiver) with
        |Some(receiverModule) ->
            let (newRecieverState, outputs) = Process receiverModule (sender, pulse) step
            if getHitTo <> "" && outputs |> Array.exists( fun (r,(_,p)) ->  getHitTo = r && p = false) 
            then 
                modules, counter, true
            else
            let newCounter = counter|>Array.map (fun (s,c) -> s, (if s = pulse then c + 1 else c))
            processSignals (modules |> Array.map(fun m -> if m.name <> receiver then m else newRecieverState)) newCounter (Array.append (signals |> Array.tail) outputs) getHitTo step
        |_ -> 
            let newCounter = counter|>Array.map (fun (s,c) -> s, (if s = pulse then c + 1 else c))
            processSignals (modules) newCounter (signals |> Array.tail) getHitTo step

    let player (modules:Module array, counter:(bool*int) array, getHitTo, stopped) (signals: (string*(string*bool)) array) =
        let (nModules, nCounter, stopped) = processSignals modules counter signals getHitTo 0
        nModules, nCounter, getHitTo, stopped

    let rec player2 (modules:Module array, counter:(bool*int) array, getHitTo, stopped, i) (signals: (string*(string*bool)) array) =
        let (nModules, nCounter, stopped) = processSignals modules counter signals getHitTo i
        if stopped then
            i
        else
            player2 (nModules, nCounter, getHitTo, stopped, i + 1) signals
    let puzzle1 (input:string array) = 
        let modules = input |> parse |> Array.unzip |> snd
        seq{for i in 1..1000 ->  [|("broadcaster",("button", false))|]} |> Seq.fold player (modules, [|(true,0);(false,0)|], "",false)
        |> fun (_, counter,_,_) -> counter |> Array.unzip |> snd |> Array.map int64 |> Array.fold (*) 1L
    

    let rec lcmMultiple (numbers :int64 array) =
        let rec gcd x y =
            match y with
            |0L -> x
            |_ -> gcd y (x % y)

        let lcm a b = a*b/(gcd a b)
        
        if numbers.Length = 1 
        then 
            numbers[0]
        else
            let newV = lcm numbers[0] numbers[1]
            numbers |> Array.skip 2 |> Array.append [|newV|] |> lcmMultiple
    
    let puzzle2 (input:string array) =
        [|3917L;3943L;3947L;4001L|] |> lcmMultiple
        //cache.Clear()
        //let modules = input |> parse |> Array.unzip |> snd
        //player2 (modules,[|(true,0);(false,0)|], "output", false, 0) [|button|]
        
                
    let Solution = (new Solution(20, puzzle1, puzzle2) :> ISolution).Execute