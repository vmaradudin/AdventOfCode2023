namespace ``2023``

open AdventOfCode2023.Day11
open Xunit

module ``Day 11`` =
    let testInput = 
        [|
            "...#......"
            ".......#.."
            "#........."
            ".........."
            "......#..."
            ".#........"
            ".........#"
            ".........."
            ".......#.."
            "#...#....."
        |]
    
    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(374L, puzzle1 testInput)
    
    [<Theory>]
    [<InlineData(10L, 1030L)>]
    [<InlineData(100L, 8410L)>]
    let ``Puzzle 2`` (expander, expected) =
        Assert.Equal(expected, computeDistance expander testInput)      