namespace ``2023``

open AdventOfCode2023.Day06
open Xunit

module ``Day 06`` =
    let testInput = 
        [|
            "Time:      7  15   30"
            "Distance:  9  40  200"
        |]  
    
    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(288L, puzzle1 testInput)
    
    [<Fact>]
    let ``Puzzle 2`` () =
        Assert.Equal(71503L, puzzle2 testInput)      