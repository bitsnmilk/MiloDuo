module Tests

open Xunit
open MiloDuo

let testData = """
# Hello World

## Paragraph 2

This is **my** _markdown_ file!
"""

[<Fact>]
let ``Test that it can split a file into paragraphs``() =
    let expected = 3
    let actual = testData |> Parser.parse |> AST.paragraphs |> List.length

    Assert.Equal(expected, actual)