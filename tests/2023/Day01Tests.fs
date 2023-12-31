namespace ``2023``

open AdventOfCode2023.Day01
open Xunit

module ``Day 01``=
    let testInput1 = 
        [|
          "1abc2"
          "pqr3stu8vwx"
          "a1b2c3d4e5f"
          "treb7uchet"
        |]  
    
    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(142, puzzle1 testInput1)
    
    let testInput2 =
        [|
            "two1nine"
            "eightwothree"
            "abcone2threexyz"
            "xtwone3four"
            "4nineeightseven2"
            "zoneight234"
            "7pqrstsixteen"
        |]

    [<Fact>]
    let ``Puzzle 2`` () =
        Assert.Equal(281, puzzle2 testInput2)      