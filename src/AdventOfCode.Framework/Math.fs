namespace Common

open System.IO

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverageAttribute>]
module Math =
    let rec gcd x y =
            match y with
            |0L -> x
            |_ -> gcd y (x % y)
    
    let LCM a b = a*b/(gcd a b)

    let rec arrayLCM (numbers :int64 array) =
        if numbers.Length = 1 
        then 
            numbers[0]
        else
            let newV = LCM numbers[0] numbers[1]
            numbers |> Array.skip 2 |> Array.append [|newV|] |> arrayLCM

