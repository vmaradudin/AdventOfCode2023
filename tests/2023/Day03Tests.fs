namespace ``2023``

open AdventOfCode2023.Day03
open Xunit

module ``Day 03`` =
    let testInput = 
        [|
            "467..114.."
            "...*......"
            "..35..633."
            "......#..."
            "617*......"
            ".....+.58."
            "..592....."
            "......755."
            "...$.*...."
            ".664.598.."
        |]  
    
    [<Fact>]
    let ``Puzzle 1`` () =
        Assert.Equal(4361, puzzle1 testInput)
    
    [<Fact>]
    let ``Puzzle 2`` () =
        Assert.Equal(467835, puzzle2 testInput)      