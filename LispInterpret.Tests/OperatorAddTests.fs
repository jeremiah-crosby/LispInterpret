namespace Tests

open NUnit.Framework
open FsUnit
open Lexer
open Parser
open Interpreter

type OperatorAddTests () =

    [<Test>]
    member this.``(+) should be error`` () =
        let tokens = lex "(+)"
        let parsed = parse tokens
        let result = parsed |> List.head |> evalExpression
        result |> should equal (ErrorResult("At least 2 numeric arguments required"))

    [<Test>]
    member this.``(+ 5 5) = 10`` () =
        let tokens = lex "(+ 5 5)"
        let parsed = parse tokens
        let result = parsed |> List.head |> evalExpression
        result |> should equal (IntResult(10))

    [<Test>]
    member this.``(+ 5 5 5) = 15`` () =
        let tokens = lex "(+ 5 5 5)"
        let parsed = parse tokens
        let result = parsed |> List.head |> evalExpression
        result |> should equal (IntResult(15))

    [<Test>]
    member this.``(+ (+ 5 5) 5) = 15`` () =
        let tokens = lex "(+ (+ 5 5) 5)"
        let parsed = parse tokens
        let result = parsed |> List.head |> evalExpression
        result |> should equal (IntResult(15))

    [<Test>]
    member this.``(+ (5 5 5)) = 15`` () =
        let tokens = lex "(+ (5 5 5))"
        let parsed = parse tokens
        let result = parsed |> List.head |> evalExpression
        result |> should equal (IntResult(15))

    [<Test>]
    member this.``(+ 2.34 4) = 6.34`` () =
        let tokens = lex "(+ 2.34 4)"
        let parsed = parse tokens
        let result = parsed |> List.head |> evalExpression
        result |> should equal (FloatResult(6.34))