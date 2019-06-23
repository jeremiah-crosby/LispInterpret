namespace Tests

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
        actual |> should equal [ErrorExpr "Expected ')'"]

    [<Test>]
    member this.``should parse an int`` () =
        parse [Number(Some(500), None)] |> should equal [IntExpr(500)]

    [<Test>]
    member this.``should parse a float`` () =
        parse [Number(None, Some(64.3))] |> should equal [FloatExpr(64.3)]

    [<Test>]
    member this.``should parse multiple expressions`` () =
        let actual = parse [
            LeftParenthesis;
            Symbol "someFunc";
            Number(Some(5), None);
            Number(Some(6), None);
            Number(Some(7), None);
            RightParenthesis;
            Number(Some(100), None);
            Number(Some(200), None)]
        actual |> should equal [
            ListExpr [SymbolExpr "someFunc"; IntExpr(5); IntExpr(6); IntExpr(7)];
            IntExpr(100);
            IntExpr(200)]

    [<Test>]
    member this.``should parse empty list`` () =
        let actual = parse [LeftParenthesis; RightParenthesis;]
        actual |> should equal [ListExpr []]

    [<Test>]
    member this.``should parse non-empty list of atoms`` () =
        let actual = parse [LeftParenthesis; Symbol "someFunc"; Number(Some(5), None); Number(Some(6), None); Number(Some(7), None); RightParenthesis]
        actual |> should equal [ListExpr [SymbolExpr "someFunc"; IntExpr(5); IntExpr(6); IntExpr(7)]]

    [<Test>]
    member this.``should parse non-empty list with list members`` () =
        let actual = parse [
            LeftParenthesis;
            LeftParenthesis;
            Number(Some(5), None);
            Number(Some(2), None);
            RightParenthesis;
            LeftParenthesis;
            Symbol "x"
            RightParenthesis;
            Number(Some(10), None)
            RightParenthesis;]
        actual |> should equal [
            ListExpr [
                ListExpr [IntExpr(5); IntExpr(2)];
                ListExpr [SymbolExpr("x")];
                IntExpr(10)]]