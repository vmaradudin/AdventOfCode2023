namespace ``2023``

open AdventOfCode2023.Day07
open Xunit

module ``Day 07`` =
    let testInput = 
        [|
            "32T3K 765"
            "T55J5 684"
            "KK677 28"
            "KTJJT 220"
            "QQQJA 483"
        |]  
    
    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(6440, puzzle1 testInput)
    
    [<Fact>]
    let ``Puzzle 2`` () =
        Assert.Equal(5905, puzzle2 testInput)      