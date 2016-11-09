module MiloDuo

open System
open System.Text.RegularExpressions

module private String =
    let Trim (s: string) =
        s.Trim()

module private Regex =
    let Split p i =
        Regex.Split(i, p)

module AST =
    type Link = string
    type Span =
    | Literal of string
    | Emphasis of Span
    | Bold of Span
    | Href of Link * Span

    type Header =
    | H1 of Span list
    | H2 of Span list
    | H3 of Span list
    | H4 of Span list
    | H5 of Span list
    | H6 of Span list

    type Paragraph =
    | Paragraph of Span list
    | Header of Header

    type Document = Body of Paragraph list

    let paragraphs d =
        match d with
        | Body paragraphs -> paragraphs

module Tokenizer =
    let tokenize s =
        "foo"

// module Parser =
//     open AST
//     type Progress = { parsed : char list; rest : char list }

//     let parse (s : string) =
//         let parse

        // Body [
        //     Header (H1 [Literal "hello world"])
        // ]
