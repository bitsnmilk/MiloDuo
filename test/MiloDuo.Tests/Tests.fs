module MiloDuo.UnitTests

open Xunit
open MiloDuo

let testData = """
# Hello World

## Paragraph 2

This is my markdown file!
"""

[<Fact>]
let ``Test that it can split a file into paragraphs``() =
    let expected = 3
    let actual = parse(testData) |> Seq.length

    Assert.Equal(expected, actual)

[<Fact>]
let ``Test that it detects the first block is an h1``() =
    let actual = parse(testData) |> Seq.tryHead
    let isHeader = actual
                    |> Option.bind (fun p -> match p with
                                                | Header header -> Some header
                                                | _ -> None)
                    |> Option.bind (fun h -> match h with | H1 _ -> Some true | _ -> None)
                    |> Option.isSome


    Assert.Equal(isHeader, true)

[<Fact>]
let ``Test that it detects the second block is an h2``() = 
    let actual = testData |> parse |> Seq.tryItem 1
    let isHeader = actual
                    |> Option.bind (fun p -> match p with
                                                | Header header -> Some header
                                                | _ -> None)
                    |> Option.bind (fun h -> match h with | H2 _ -> Some true | _ -> None)
                    |> Option.isSome

    Assert.Equal(isHeader, true)
