namespace AdventOfCode2023

open Common.Types

module Day07 =
    type HandType =
    |FiveOfAKind  = 7 
    |FourOfAKind = 6
    |FullHouse = 5
    |ThreeOfAKind = 4
    |TwoPair = 3
    |OnePair = 2
    |HighCard = 1

    let cardMapper useJocker c =
        match c with
        |'T' -> 10uy
        |'J' -> if useJocker then 1uy else 11uy
        |'Q' -> 12uy
        |'K' -> 13uy
        |'A' -> 14uy
        |o -> o |>string|> byte

    type Hand = 
        { 
            Type : HandType;
            Cards: string;
            Bid: int;
        }

    let getHandType cards = 
        let groups = 
            cards 
            |> Seq.countBy id
            |> fun ns -> (ns|> Seq.tryFind (fun (c,_) -> c = 1uy) |> function |Some((_,x)) -> x |_ -> 0) , (ns |> Seq.filter (fun (c,_) -> c <> 1uy) |> Seq.map snd |> Seq.sortDescending |> Array.ofSeq)
            |> function 
                |(j,a) when (a |> Array.length) = 0 -> [|j|]  
                |(j,a) -> ([|a[0] + j|], (a |> Array.tail)) ||> Array.append

        match groups[0] with
            |5 -> HandType.FiveOfAKind
            |4 -> HandType.FourOfAKind
            |3 when groups[1] = 2 -> HandType.FullHouse
            |3 -> HandType.ThreeOfAKind
            |2 when groups[1] = 2 -> HandType.TwoPair
            |2 -> HandType.OnePair
            |_ -> HandType.HighCard

    let getHand (cards, bid) =
        { 
            Type = getHandType cards
            Cards = cards |> Seq.map _.ToString().PadLeft(2, '0') |> String.concat ""; 
            Bid = bid;
        }

    let parseInput useJocker (input:string array)  =
        input 
        |> Array.map (fun b -> b.Split(' ') |> fun a -> (a[0] |> Seq.map (cardMapper useJocker), a[1] |> int) |> getHand)

    let puzzle1 input = 
        input 
        |> parseInput false
        |> Array.sort
        |> Array.mapi (fun i v -> (int(i) + 1) * v.Bid)
        |> Array.sum

    let puzzle2 (input:string array) = 
        input 
        |> parseInput true
        |> Array.sort
        |> Array.mapi (fun i v -> (int(i) + 1) * v.Bid)
        |> Array.sum

    let Solution = (new Solution(7, puzzle1, puzzle2) :> ISolution).Execute
