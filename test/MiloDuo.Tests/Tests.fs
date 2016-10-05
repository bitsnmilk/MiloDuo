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
    let actual = testData |> parse |> paragraphs |> Seq.length

    Assert.Equal(expected, actual)

[<Fact>]
let ``Test that it detects the first block is an h1``() =
    let actual = testData |> parse |> tryHead
    let isHeader = actual
                    |> Option.bind tryCastHeader
                    |> Option.bind (fun h -> match h with | H1 _ -> Some true | _ -> None)
                    |> Option.isSome


    Assert.Equal(isHeader, true)

[<Fact>]
let ``Test that it detects the second block is an h2``() =
    let actual = testData |> parse |> paragraphs |> Seq.tryItem 1

    let isHeader = actual
                    |> Option.bind tryCastHeader
                    |> Option.bind (fun h -> match h with | H2 _ -> Some true | _ -> None)
                    |> Option.isSome

    Assert.Equal(isHeader, true)
