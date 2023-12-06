namespace AdventOfCode2023

open Common.Types
open System.Text.RegularExpressions

module Day04 =
    
    let parseCard (input:string array) =
        input |> Array.map (fun a -> a.Split([|':'; '|'|])) |> Array.map (fun a -> 
        (a[0], (a[1] |> _.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)|> Array.map int), (a[2]|> _.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)|>Array.map int)
        ))

    let playCard (cardName, card, game) =
        let res = game |> Array.fold (fun st a -> match a with |a1 when (card |> Array.contains a1) -> (if st = 0 then 1 else st*2) |_ -> st) 0
        res
  

    let playCard2 (cardName, card, game) =
        let res = game |> Array.fold (fun st a -> match a with |a1 when (card |> Array.contains a1) -> (if st = 0 then 1 else st+1) |_ -> st) 0
        res

    let rec countCards (cards:(int*int) array) acc =
        if cards |> Array.isEmpty then acc
        else
            let current = cards |> Array.head
            let times = current |> fst
            let score = current |> snd
            let newAcc = acc + times
            let rest = cards |> Array.tail
            let r1 = rest |> Array.take score |> Array.map (fun (t,s) -> (t + times, s))
            let r2 = rest |> Array.skip score
            countCards (Array.append r1 r2) newAcc


    let puzzle1 input = 
        input |> parseCard |> Array.map playCard |> Array.sum

    let puzzle2 input = 
        let played = input |> parseCard |> Array.map playCard2

        played |> Array.map (fun a -> (1, a)) |> fun a -> countCards a 0

    let Solution = (new Solution(4, puzzle1, puzzle2) :> ISolution).Execute