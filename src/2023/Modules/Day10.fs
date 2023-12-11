namespace AdventOfCode2023

open Common.Types

module Day10 =
    let defineS (input:string array) =
        let (sX, sY) = input |> Array.indexed |> Array.tryPick( fun (x,line) -> line.IndexOf('S') |> function |(-1) -> None |y -> Some(x,y)) |> _.Value
        let s=
            [|
                ('n', (sX - 1, sY))
                ('s', (sX + 1, sY))
                ('w', (sX, sY - 1))
                ('e', (sX, sY + 1))
            |] 
            |> Array.filter(fun (_,(x,y)) -> x >= 0 && x < input.Length && y >= 0 && y < input[0].Length)
            |> Array.map (fun (d,(x,y)) -> (d, input[x][y]))
            |> Array.choose 
                (function 
                 |('n', s) when s='|' || s='7' || s='F' -> Some('n')
                 |('s', s) when s='|' || s='L' || s='J' -> Some('s')
                 |('w', s) when s='-' || s='L' || s='F' -> Some('w')
                 |('e', s) when s='-' || s='J' || s='7' -> Some('e')
                 |_ -> None)
            |> fun a -> 
               match a with
               |f when (f|> Array.contains 'n') ->
                    match f |> Array.except([|'n'|]) |> Array.head with
                    |'s' -> '|'
                    |'w' -> 'J'
                    |'e' -> 'L'
               |f when (f|> Array.contains 'w') ->
                    match f |> Array.except([|'w'|]) |> Array.head with
                    |'s' -> '7'
                    |'e' -> '-'
               |f when (f|> Array.contains 'e') ->
                    match f |> Array.except([|'e'|]) |> Array.head with
                    |'s' -> 'F'
        input |> Array.mapi (fun x line -> if x <> sX then line else line.Replace('S', s))
        |> fun i -> (sX,sY),i

    let step (xp,yp) (x,y) (input:string array) =
        match input[x][y] with
        |'|' -> [| (x - 1, y); (x + 1, y) |]
        |'-' -> [| (x, y - 1); (x, y + 1) |]
        |'L' -> [| (x - 1, y); (x, y + 1) |]
        |'J' -> [| (x, y - 1); (x - 1, y) |]
        |'7' -> [| (x, y - 1); (x + 1, y) |]
        |'F' -> [| (x, y + 1); (x + 1, y) |]
        |> Array.except [|(xp,yp)|]
        |> Array.head

    let rec find (xp,yp) (x,y) (input:string array) cnt =
        if (input[x][y] = '*')
        then 
            let res = input |> Array.map (fun a -> a|> Seq.map (function |'*' -> "*" |_ -> " ") |> Seq.fold (+) "")
            (cnt, res)
        else
           let newPoint = step (xp,yp) (x,y) input
           let newInput = input |> Array.mapi (fun i v -> (if i = x then v.Remove(y,1).Insert(y,"*") else v))
           find (x,y) newPoint newInput (cnt+1)

    let map3 =
        function
        |'|' -> [|".*.";".*.";".*."|]
        |'-' -> [|"...";"***";"..."|]
        |'L' -> [|".*.";".**";"..."|]
        |'J' -> [|".*.";"**.";"..."|] 
        |'7' -> [|"...";"**.";".*."|]
        |'F' -> [|"...";".**";".*."|]
        |_ ->   [|"III";"III";"III"|]
    
    let map3Folder (result: string array) (input:string array)=
        [|result[0] + input[0]; result[1] + input[1]; result[2] + input[2]|]

    let mapLine (line:string) =
        line |> Seq.map map3 |> Seq.fold map3Folder [|"";"";""|]

    let rec floodFill (unvisited: (int*int) array) (input:string array) =
        let (x,y) = unvisited |> Array.head
        let add = 
            [|(x-1,y); (x+1,y); (x,y-1); (x,y+1)|] 
            |> Array.filter(fun (a,b) -> 
                a >= 0 && 
                a < (input.Length) && 
                b>=0 && 
                b< (input[0].Length) && 
                (unvisited |> Array.contains((a,b)) |> not) && 
                input[a][b] <> 'O' && input[a][b] <> '*')
        let newUnvisited = Array.append (unvisited |> Array.tail) add
        let newInput = input |> Array.mapi (fun i line -> if i <> x then line else line.Remove(y,1).Insert(y,"O"))
        if (newUnvisited |> Array.isEmpty)
        then 
            newInput
        else
            floodFill newUnvisited newInput

    let puzzle1 (input:string array) = 
        let (start, fullInput) = defineS input
        find (-1,-1) start fullInput 0 |> fst |> fun a -> a / 2

    let puzzle2 input = 
        let (start, fullInput) = defineS input
        let mask = find (-1,-1) start fullInput 0 |> snd
        let cleanInput = 
            fullInput |> 
                Array.mapi (fun i line -> (Seq.map2 (fun s m -> match m with |'*' -> s.ToString() |_ -> " " ) line mask[i]) |> Array.ofSeq |> String.concat "")
        cleanInput |> Array.map mapLine |> Array.collect id |> floodFill [|0,0|] |> String.concat "" |> Seq.sumBy (fun a -> if a = 'I' then 1 else 0) |> fun r -> r / 9
    
    let Solution = (new Solution(10, puzzle1, puzzle2) :> ISolution).Execute
