namespace Year2023.Tests

open AdventOfCode2023.Day03
open Xunit

module Day03Tests =
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
    let ``Day 3 Puzzle 1`` () =
        Assert.Equal(4361, puzzle1 testInput)
    
    [<Fact>]
    let ``Day 3 Puzzle 2`` () =
        Assert.Equal(467835, puzzle2 testInput)      