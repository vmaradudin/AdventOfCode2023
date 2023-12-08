namespace ``2023``

open AdventOfCode2023.Day08
open Xunit

module ``Day 08`` =
    let testInput1 = 
        [|
            "RL"
            ""
            "AAA = (BBB, CCC)"
            "BBB = (DDD, EEE)"
            "CCC = (ZZZ, GGG)"
            "DDD = (DDD, DDD)"
            "EEE = (EEE, EEE)"
            "GGG = (GGG, GGG)"
            "ZZZ = (ZZZ, ZZZ)"
        |]

    let testInput2 =
        [|
            "LLR"
            ""
            "AAA = (BBB, BBB)"
            "BBB = (AAA, ZZZ)"
            "ZZZ = (ZZZ, ZZZ)"
        |]
    let testInput3 =
        [|
            "LR"
            ""
            "11A = (11B, XXX)"
            "11B = (XXX, 11Z)"
            "11Z = (11B, XXX)"
            "22A = (22B, XXX)"
            "22B = (22C, 22C)"
            "22C = (22Z, 22Z)"
            "22Z = (22B, 22B)"
            "XXX = (XXX, XXX)"
        |]
    
    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(2L, puzzle1 testInput1)
        Assert.Equal(6L, puzzle1 testInput2)
    
    [<Fact>]
    let ``Puzzle 2`` () =
        Assert.Equal(6L, puzzle2 testInput3)      