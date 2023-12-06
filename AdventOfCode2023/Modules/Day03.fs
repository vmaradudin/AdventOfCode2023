namespace AdventOfCode2023

open Common.Types
open System.Text.RegularExpressions

module Day03 =

    let symbolsAdjacent i j matrix =
        let maxI = (matrix |> Array.length) - 1
        let maxJ = (matrix[i] |> Array.length) - 1
        let topLeft =      (i>0)&&(j>0)&&(matrix[i-1][j-1]<>'.')&&((matrix[i-1][j-1]<'0')||(matrix[i-1][j-1]>'9'))
        let topCenter =    (i>0)&&(matrix[i-1][j]<>'.')&&((matrix[i-1][j]<'0')||(matrix[i-1][j]>'9'))
        let topRight =     (i>0)&&(j<maxJ)&&(matrix[i-1][j+1]<>'.')&&((matrix[i-1][j+1]<'0')||(matrix[i-1][j+1]>'9'))
        let left =         (j>0)&&(matrix[i][j-1]<>'.')&&((matrix[i][j-1]<'0')||(matrix[i][j-1]>'9'))
        let right =        (j<maxJ)&&(matrix[i][j+1]<>'.')&&((matrix[i][j+1]<'0')||(matrix[i][j+1]>'9'))
        let bottomLeft =   (i<maxI)&&(j>0)&&(matrix[i+1][j-1]<>'.')&&((matrix[i+1][j-1]<'0')||(matrix[i+1][j-1]>'9'))
        let bottomCenter = (i<maxI)&&(matrix[i+1][j]<>'.')&&((matrix[i+1][j]<'0')||(matrix[i+1][j]>'9'))
        let bottomRight =  (i<maxI)&&(j<maxJ)&&(matrix[i+1][j+1]<>'.')&&((matrix[i+1][j+1]<'0')||(matrix[i+1][j+1]>'9'))
        topLeft || topCenter || topRight || left || right || bottomLeft || bottomCenter || bottomRight
    
    let gear i j (known:bool*(int*int)) matrix =
        let maxI = (matrix |> Array.length) - 1
        let maxJ = (matrix[i] |> Array.length) - 1
        if (known |> fst) 
        then 
            known
        else
            match (i,j) with
            |(i,j) when (i>0)&&(j>0)&&(matrix[i-1][j-1]<>'.')&&((matrix[i-1][j-1]<'0')||(matrix[i-1][j-1]>'9')) -> (true, (i-1,j-1))        
            |(i,j) when (i>0)&&(matrix[i-1][j]<>'.')&&((matrix[i-1][j]<'0')||(matrix[i-1][j]>'9')) -> (true, (i-1,j))
            |(i,j) when (i>0)&&(j<maxJ)&&(matrix[i-1][j+1]<>'.')&&((matrix[i-1][j+1]<'0')||(matrix[i-1][j+1]>'9')) -> (true, (i-1,j+1))
            |(i,j) when (j>0)&&(matrix[i][j-1]<>'.')&&((matrix[i][j-1]<'0')||(matrix[i][j-1]>'9')) -> (true, (i,j-1))
            |(i,j) when (j<maxJ)&&(matrix[i][j+1]<>'.')&&((matrix[i][j+1]<'0')||(matrix[i][j+1]>'9')) -> (true, (i,j+1))
            |(i,j) when (i<maxI)&&(j>0)&&(matrix[i+1][j-1]<>'.')&&((matrix[i+1][j-1]<'0')||(matrix[i+1][j-1]>'9')) -> (true, (i+1,j-1))
            |(i,j) when (i<maxI)&&(matrix[i+1][j]<>'.')&&((matrix[i+1][j]<'0')||(matrix[i+1][j]>'9')) -> (true, (i+1,j))
            |(i,j) when (i<maxI)&&(j<maxJ)&&(matrix[i+1][j+1]<>'.')&&((matrix[i+1][j+1]<'0')||(matrix[i+1][j+1]>'9')) -> (true, (i+1,j+1))
            |_ -> (false, (i,j))

    let rec scanNumbers lineNum i num isPart (matrix:char array array) res=
        match (lineNum, i) with
        |(l,_) when l = (matrix |> Array.length) -> res
        |(_,c) when c >= (matrix[lineNum] |> Array.length)  -> scanNumbers (lineNum + 1) 0 0 false matrix (res + (match isPart with |true -> num |_->0))
        |(_,_) when matrix[lineNum][i]>='0' && matrix[lineNum][i]<='9' -> scanNumbers lineNum (i+1) (num * 10 + (matrix[lineNum][i] |> int) - ('0'|>int)) (isPart || (symbolsAdjacent lineNum i matrix)) matrix res
        |_ -> scanNumbers lineNum (i+1) 0 false matrix (res + (match isPart with |true -> num |_->0))

    let rec scanNumbers2 lineNum i num (isPart:bool*(int*int)) (matrix:char array array) (res:((int*int)*int) array)=
        match (lineNum, i) with
        |(l,_) when l = (matrix |> Array.length) -> res
        |(_,c) when c >= (matrix[lineNum] |> Array.length)  -> scanNumbers2 (lineNum + 1) 0 0 (false,(0,0)) matrix (match isPart with |(true,(gi,gj)) -> Array.insertAt 0 ((gi,gj), num) res |(_,(_,_)) -> res)
        |(_,_) when matrix[lineNum][i]>='0' && matrix[lineNum][i]<='9' -> scanNumbers2 lineNum (i+1) (num * 10 + (matrix[lineNum][i] |> int) - ('0'|>int)) (gear lineNum i isPart matrix) matrix res
        |_ -> scanNumbers2 lineNum (i+1) 0 (false,(0,0)) matrix (match isPart with |(true,(gi,gj)) -> Array.insertAt 0 ((gi,gj), num) res |(_,(_,_)) -> res)

    let puzzle1 input = 
        let inp = input |> Array.map (Seq.toArray)
        scanNumbers 0 0 0 false inp 0

    let puzzle2 input = 
        let inp = input |> Array.map (Seq.map (fun a -> match a with |q when q='*' || q='.' || (q>='0' && q<='9') -> q |_ -> '.'))|> Array.map (Array.ofSeq) 
        let res = scanNumbers2 0 0 0 (false,(0,0)) inp Array.empty

        res 
        |> Array.groupBy fst 
        |> Array.filter (fun (g,ns) -> (ns |> Array.length = 2)) 
        |> Array.map (fun (g, ns) -> ((ns[0] |> snd) * (ns[1] |> snd)))
        |> Array.sum

    let Solution = (new Solution(3, puzzle1, puzzle2) :> ISolution).Execute