namespace AdventOfCode2023

open Common.Types

module Day07 =
    let cardMapper c =
        match c with
        |'T' -> 10uy
        |'J' -> 11uy
        |'Q' -> 12uy
        |'K' -> 13uy
        |'A' -> 14uy
        |o -> o |>string|> byte

    type handtype =
    |FiveOfAKind  = 7 
    |FourOfAKind = 6
    |FullHouse = 5
    |ThreeOfAKind = 4
    |TwoPair = 3
    |OnePair = 2
    |HighCard = 1

    type Hand = 
        { 
            Type : handtype;
            Cards: byte seq;
            Bid: uint64;
        }

    let getHandType cards = 
            match cards |> Seq.groupBy id with
            | a when (a |> Seq.length = 1) -> handtype.FiveOfAKind
            | a when (a |> Seq.exists (fun (_,b) -> (b |> Seq.length) = 4)) -> handtype.FourOfAKind
            | a when ((a |> Seq.exists (fun (_,b) -> (b |> Seq.length) = 3))
                   && (a |> Seq.exists (fun (_,b) -> (b |> Seq.length) = 2))) -> handtype.FullHouse
            | a when (a |> Seq.exists (fun (_,b) -> (b |> Seq.length) = 3)) -> handtype.ThreeOfAKind
            | a when ((a |> Seq.filter (fun (_,b) -> (b |> Seq.length) = 2))|> Seq.length) = 2 -> handtype.TwoPair
            | a when ((a |> Seq.filter (fun (_,b) -> (b |> Seq.length) = 2))|> Seq.length) = 1 -> handtype.OnePair
            |_ -> handtype.HighCard

    let getHand (cards, bid) =
        {Cards = cards; Bid = bid; Type = getHandType cards}



    type Hand1 = 
        { 
            Type : handtype;
            Cards: string;
        }

    let parseInput (input:string array)  =
        input 
        |> Array.map (fun b -> b.Split(' ') |> fun a -> (a[0] |>Seq.map cardMapper, a[1] |> uint64) |> getHand)
        
    let wildcard (hand:byte array) =
        let wc = [|2uy..14uy|] |> Array.filter (fun a -> a <> 11uy)
        wc |> Array.map (fun w -> hand |> Array.map(fun r -> if (r = 11uy) then w else r))
        
    let getHand2 (cards, bid) =
        {
            Cards = cards |> Seq.map (fun a -> if a = 11uy then 1uy else a); 
            Bid=bid; 
            Type = cards |> wildcard |> Array.map getHandType |> Array.max }

    let puzzle1 input = 
        input 
        |> parseInput
        |> Array.map (fun a -> {Type = a.Type; Cards = a.Cards |> Seq.map (fun c -> c|> string |> _.PadLeft(2,'0')) |> String.concat "" }, a.Bid)
        |> Array.sortBy fst 
        |> Array.mapi (fun i (_,v) -> ((i + 1) |> uint64) * v)
        |> Array.sum

    let puzzle2 (input:string array) = 
        input 
        |> parseInput
        |> Array.map (fun a -> (getHand2 ((a.Cards |> Array.ofSeq), a.Bid)))
        |> Array.map (fun a -> {Type = a.Type; Cards = a.Cards |> Seq.map (fun c -> c|> string |> _.PadLeft(2,'0')) |> String.concat "" }, a.Bid)
        |> Array.sortBy fst 
        |> Array.mapi (fun i (_,v) -> ((i + 1) |> uint64) * v)
        |> Array.sum

    let Solution = (new Solution(7, puzzle1, puzzle2) :> ISolution).Execute
