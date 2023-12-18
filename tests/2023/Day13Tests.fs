namespace ``2023``

open AdventOfCode2023.Day13
open Xunit

module ``Day 13`` =
    let testInput = 
        [|
        "#.##..##."
        "..#.##.#."
        "##......#"
        "##......#"
        "..#.##.#."
        "..##..##."
        "#.#.##.#."
        ""
        "#...##..#"
        "#....#..#"
        "..##..###"
        "#####.##."
        "#####.##."
        "..##..###"
        "#....#..#"           
        |]
    
    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(405, puzzle1 testInput)
    
    [<Fact>]
    let ``Puzzle 2`` () =
        Assert.Equal(400, puzzle2 testInput)      