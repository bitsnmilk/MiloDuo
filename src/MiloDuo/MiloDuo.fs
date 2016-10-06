module MiloDuo

open System
open System.Text.RegularExpressions

module private String =
    let Trim (s: string) =
        s.Trim()

module private Regex =
    let Split p i =
        Regex.Split(i, p)

type Link = string
type Span =
    | Literal of string
    | Emphasis of string
    | Bold of string
    | Href of Link * string


let appendCharToString = sprintf "%s%c"

type Result<'T, 'U> =
    | Success of 'T
    | Failure of 'U

let parseLiteral (s : string) =

    let matcher (token : string) tokens currentGroup =
        let headMatchesToken = tokens |> List.tryHead |> Option.map ((=) token)
        match token with
        | "__" ->
            if headMatchesToken = Some(true) then Success (Emphasis currentGroup, tokens |> List.tail)
            else Failure (List.Cons (token, tokens))
        | "**" ->
            if headMatchesToken = Some(true) then Success (Bold currentGroup, tokens |> List.tail)
            else Failure (List.Cons (token, tokens))
        | _ -> Failure tokens

    let rec parseStr (spans : Span List) (tokens : string List) (currentGroup : string) remaining =
        match remaining with
        | '_'::'_'::rest ->
            let result = matcher "__" tokens currentGroup
            match result with
            | Success(span, tokens) -> parseStr (List.Cons (span, spans)) tokens "" rest
            | Failure(tokens) -> parseStr spans tokens currentGroup rest
        | '*'::'*'::rest ->
            let result = matcher "**" tokens currentGroup
            match result with
            | Success(span, tokens) -> parseStr (List.Cons (span, spans)) tokens "" rest
            | Failure(tokens) -> parseStr spans tokens currentGroup rest
        | head::tail -> parseStr spans tokens (appendCharToString currentGroup head) tail
        | [] -> spans

    parseStr [] [] "" (s |> Seq.toList)

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
    | '#'::'#'::'#'::'#'::'#'::'#'::rest -> rest.ToString() |> parseLiteral |> H6 |> Header
    | '#'::'#'::'#'::'#'::'#'::rest -> rest.ToString() |> parseLiteral |> H5 |> Header
    | '#'::'#'::'#'::'#'::rest -> rest.ToString() |> parseLiteral |> H4 |> Header
    | '#'::'#'::'#'::rest -> rest.ToString() |> parseLiteral |> H3 |> Header
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
