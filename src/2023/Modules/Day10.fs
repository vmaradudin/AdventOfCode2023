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
            |> function
               |[|'n'; 's'|] | [|'s'; 'n'|] -> '|'
               |[|'n'; 'w'|] | [|'w'; 'n'|] -> 'J'
               |[|'n'; 'e'|] | [|'e'; 'n'|] -> 'L'
               |[|'w'; 's'|] | [|'s'; 'w'|] -> '7'
               |[|'w'; 'e'|] | [|'e'; 'w'|] -> '-'
               |[|'s'; 'e'|] | [|'e'; 's'|] -> 'F'
               |_ -> ' '

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
        |_ -> [||]
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
   
    let rec getLine (input:string array) start direction (stopAtS:bool) =
        let x,y = start
        let finish, d =
            match direction, input[x][y] with
            |_  , 'S' when stopAtS -> start, 'S'
            |'U', 'F' -> start, 'R'
            |'U', '7' -> start, 'L'
            |'U', _   -> getLine input (x - 1, y) 'U' true
            |'R', '7' -> start, 'D'
            |'R', 'J' -> start, 'U'
            |'R', _   -> getLine input (x, y + 1) 'R' true
            |'D', 'J' -> start, 'L'
            |'D', 'L' -> start, 'R'
            |'D', _   -> getLine input (x + 1, y) 'D' true
            |'L', 'L' -> start, 'U'
            |'L', 'F' -> start, 'D'
            |'L', _   -> getLine input (x, y - 1) 'L' true
            
        finish, d

    let rec GetLines (square:int) (input:string array) (start:(int*int)) (direction:char) =
        let ((xs,ys),((xf,yf),d)) = (start), (getLine input start direction false)
        let newSquare =
            square + 
            match (direction) with
            |'R' -> yf - ys
            |'L' -> ys - yf
            |'U' -> xs - xf
            |'D' -> xf - xs
        if (input[xf][yf] = 'S') 
        then square + 1
        else
        GetLines newSquare input (xf,yf) d


    let rec getLines2 (res:(char*int) array) (input:string array) (start:(int*int)) (direction:char) =
        let ((xs,ys),((xf,yf),d)) = (start), (getLine input start direction false)
        if (input[xf][yf] = 'S') 
            then res
        else
        getLines2 (Array.append res [|direction, abs((xf - xs) + (yf - ys))|]) input (xf,yf) d
   
    let calculateSquare (square: int*(int*int)) (direction, steps) : (int*(int*int))=
        let (sum,(x,y)) = square
        match direction with
        | 'R' | '0' -> sum + steps * x,        (x, y + steps)
        | 'L' | '2' -> sum - steps * (x - 1),  (x, y - steps)
        | 'D' | '1' -> sum + steps,            (x - steps, y)
        | 'U' | '3' -> sum,                    (x + steps, y)
        |_ -> sum, (x, y)
   
    let puzzle1 (input:string array) = 
        let (start, fullInput) = defineS input
        find (-1,-1) start fullInput 0 |> fst |> fun a -> a / 2

    let puzzle2 (input:string array) = 
        let s = input |> Array.indexed |> Array.tryPick( fun (x,line) -> line.IndexOf('S') |> function |(-1) -> None |y -> Some(x,y)) |> _.Value
        let i = getLines2 [||] input s 'R'
        let r = i |> Array.sumBy (snd)
        let sq = i |> Array.fold calculateSquare (1,(0, 0))
        GetLines 1 input s 'R'
    
    let Solution = (new Solution(10, puzzle1, puzzle2) :> ISolution).Execute
