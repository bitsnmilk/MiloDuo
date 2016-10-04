module MiloDuo

open System
open System.Text.RegularExpressions

type Header =
    | H1 of string
    | H2 of string
    | H3 of string
    | H4 of string
    | H5 of string
    | H6 of string

type Paragraph =
    | Header of Header
    | Normal of string

let parseParagraph (input: string) =
    match input |> Seq.toList with
    | '#'::'#'::rest -> rest.ToString() |> H2 |> Header
    | '#'::rest -> rest.ToString().Trim() |> H1 |> Header
    | _ -> Normal input

let parse (input: string) =
    let paragraphs = Regex.Split(input.Trim(), "\n\n")
    paragraphs |> Seq.map parseParagraph
