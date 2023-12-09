namespace ``2023``

open AdventOfCode2023.Day09
open Xunit

module ``Day 09`` =
    let testInput = 
        [|
            "0 3 6 9 12 15"
            "1 3 6 10 15 21"
            "10 13 16 21 30 45"
        |]
    
    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(114, puzzle1 testInput)
    
    [<Fact>]
    let ``Puzzle 2`` () =
        Assert.Equal(2, puzzle2 testInput)      