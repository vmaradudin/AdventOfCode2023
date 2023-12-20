namespace ``2023``

open AdventOfCode2023.Day20
open Xunit

module ``Day 20`` =
    let testInput1 = 
        [|
        "broadcaster -> a, b, c"
        "%a -> b"
        "%b -> c"
        "%c -> inv"
        "&inv -> a"
        |]

    let testInput2 = 
        [|
        "broadcaster -> a"
        "%a -> inv, con"
        "&inv -> b"
        "%b -> con"
        "&con -> output"
        |]

    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(32000000L, puzzle1 testInput1)
        Assert.Equal(11687500L, puzzle1 testInput2) 