namespace Tests

open NUnit.Framework
open FsUnit
open Lexer
open Parser
open Syntax
open Interpreter

type OperatorAddTests () =

    [<Test>]
    member this.``(+) should be error`` () =
        let (result, _) = TestHelpers.evalString "(+)"
        result |> should equal (ErrorExpr("At least 2 numeric arguments required"))

    [<Test>]
    member this.``(+ 5 5) = 10`` () =
        let (result, _) = TestHelpers.evalString "(+ 5 5)"
        result |> should equal (IntExpr(10))

    [<Test>]
    member this.``(+ 5 5 5) = 15`` () =
        let (result, _) = TestHelpers.evalString "(+ 5 5 5)"
        result |> should equal (IntExpr(15))

    [<Test>]
    member this.``(+ (+ 5 5) 5) = 15`` () =
        let (result, _) = TestHelpers.evalString "(+ (+ 5 5) 5)"
        result |> should equal (IntExpr(15))

    [<Test>]
    member this.``add list throws 2 numeric args required`` () =
        let (result, _) = TestHelpers.evalString "(+ (5 5 5))"
        result |> should equal (ErrorExpr("At least 2 numeric arguments required"))

    [<Test>]
    member this.``(+ 2.34 4) = 6.34`` () =
        let (result, _) = TestHelpers.evalString "(+ 2.34 4)"
        result |> should equal (FloatExpr(6.34))

    [<Test>]
    member this.``(+ 2 "a string") should return error`` () =
        let (result, _) = TestHelpers.evalString "(+ 2 \"a string\")"
        result |> should equal (ErrorExpr("All arguments must be numeric"))

    [<Test>]
    member this.``add 2 variables`` () =
        let (result, _) = TestHelpers.evalString @"
            (set x 5)
            (set y 6)
            (+ x y)
        "
        result |> should equal (IntExpr 11)