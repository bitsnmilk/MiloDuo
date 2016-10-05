module MiloDuo

open System
open System.Text.RegularExpressions

module private String =
    let Trim (s: string) =
        s.Trim()

module private Regex =
    let Split p i =
        Regex.Split(i, p)

type Span =
    | Literal of string

let parseLiteral s =
    [ Literal s ]

type Header =
    | H1 of Span List
    | H2 of Span List
    | H3 of Span List
    | H4 of Span List
    | H5 of Span List
    | H6 of Span List

type Paragraph =
    | Header of Header
    | Basic of Span List

type Document =
    | Body of Paragraph List

let private parseParagraph i =
    match i |> Seq.toList with
    | '#'::'#'::rest -> rest.ToString() |> parseLiteral |> H2 |> Header
    | '#'::rest -> rest.ToString().Trim() |> parseLiteral |> H1 |> Header
    | _ -> i |> parseLiteral |> Basic

let parse =
    String.Trim
    >> Regex.Split "\n\n"
    >> Seq.map parseParagraph
    >> Seq.toList
    >> Body

let tryCastHeader p =
    match p with
    | Header header -> Some header
    | _ -> None

let tryHead d =
    match d with
    | Body paragraphs -> Seq.tryHead paragraphs

let paragraphs d =
    match d with
    | Body paragraphs -> paragraphs
