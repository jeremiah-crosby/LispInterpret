﻿namespace Tests

open NUnit.Framework
open FsUnit
open Syntax

type MathOperatorTests () =

    [<Test>]
    member this.``(+) should be error`` () =
        let result = TestHelpers.evalString "(+)"
        result |> should equal (ErrorExpr("At least 2 numeric arguments required"))

    [<Test>]
    member this.``(+ 5 5) = 10`` () =
        let result = TestHelpers.evalString "(+ 5 5)"
        result |> should equal (IntExpr(10))

    [<Test>]
    member this.``(+ 5 5 5) = 15`` () =
        let result = TestHelpers.evalString "(+ 5 5 5)"
        result |> should equal (IntExpr(15))

    [<Test>]
    member this.``(+ (+ 5 5) 5) = 15`` () =
        let result = TestHelpers.evalString "(+ (+ 5 5) 5)"
        result |> should equal (IntExpr(15))

    [<Test>]
    member this.``add list throws 2 numeric args required`` () =
        let result = TestHelpers.evalString "(+ (5 5 5))"
        result |> should equal (ErrorExpr("At least 2 numeric arguments required"))

    [<Test>]
    member this.``(+ 2.34 4) = 6.34`` () =
        let result = TestHelpers.evalString "(+ 2.34 4)"
        result |> should equal (FloatExpr(6.34))

    [<Test>]
    member this.``(+ 2 "a string") should return error`` () =
        let result = TestHelpers.evalString "(+ 2 \"a string\")"
        result |> should equal (ErrorExpr("All arguments must be numeric"))

    [<Test>]
    member this.``add 2 variables`` () =
        let result = TestHelpers.evalString @"
            (set x 5)
            (set y 6)
            (+ x y)
        "
        result |> should equal (IntExpr 11)

    [<Test>]
    member this.``(* 8 8) = 64`` () =
        let result = TestHelpers.evalString "(* 8 8)"
        result |> should equal (IntExpr 64)

    [<Test>]
    member this.``(- 8 8) = 0`` () =
        let result = TestHelpers.evalString "(- 8 8)"
        result |> should equal (IntExpr 0)

    [<Test>]
    member this.``(/ 8 4) = 2`` () =
        let result = TestHelpers.evalString "(/ 8 4)"
        result |> should equal (IntExpr 2)

    [<Test>]
    member this.``(/ 8.25 3) = 2.75`` () =
        let result = TestHelpers.evalString "(/ 8.25 3)"
        result |> should equal (FloatExpr 2.75)