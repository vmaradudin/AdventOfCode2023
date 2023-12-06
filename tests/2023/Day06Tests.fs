namespace Year2023.Tests

open AdventOfCode2023.Day06
open Xunit

module Day06Tests =
    let testInput = 
        [|
            "Time:      7  15   30"
            "Distance:  9  40  200"
        |]  
    
    [<Fact>]
    let ``Day 6 Puzzle 1`` () =
        Assert.Equal(288L, puzzle1 testInput)
    
    [<Fact>]
    let ``Day 6 Puzzle 2`` () =
        Assert.Equal(71503L, puzzle2 testInput)      