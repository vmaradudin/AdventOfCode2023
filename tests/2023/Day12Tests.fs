namespace ``2023``

open AdventOfCode2023.Day12
open Xunit

module ``Day 12`` =
    let testInput = 
        [|
            "???.### 1,1,3"
            ".??..??...?##. 1,1,3"
            "?#?#?#?#?#?#?#? 1,3,1,6"
            "????.#...#... 4,1,1"
            "????.######..#####. 1,6,5"
            "?###???????? 3,2,1"
        |]
    
    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(21L, puzzle1 testInput)
    
    [<Fact>]
    let ``Puzzle 2`` () =
        Assert.Equal(525152L, puzzle2 testInput)      