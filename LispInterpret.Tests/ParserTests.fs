﻿namespace Tests

open NUnit.Framework
open FsUnit
open Syntax
open Lexer
open Parser

type ParserTests () =
    
    [<Test>]
    member this.``should parse empty`` () =
        parse [] |> should equal []

    [<Test>]
    member this.``should error on missing right parenthesis`` () =
        let actual = parse [LeftParenthesis; Symbol "x"; Number(None, Some(1.0))]
        actual |> should equal [ErrorExpr "Incomplete function call"]

    [<Test>]
    member this.``should parse (someFunc 5 6 7)`` () =
        let actual = parse [LeftParenthesis; Symbol "someFunc"; Number(Some(5), None); Number(Some(6), None); Number(Some(7), None); RightParenthesis]
        actual |> should equal [InvokeExpr("someFunc", [IntExpr(5); IntExpr(6); IntExpr(7)])]

    [<Test>]
    member this.``should parse an int`` () =
        parse [Number(Some(500), None)] |> should equal [IntExpr(500)]

    [<Test>]
    member this.``should parse a float`` () =
        parse [Number(None, Some(64.3))] |> should equal [FloatExpr(64.3)]