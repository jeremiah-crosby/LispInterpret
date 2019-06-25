namespace Tests

open NUnit.Framework
open FsUnit
open Lexer
open Parser
open Interpreter

type InterpreterTests () =

    [<Test>]
    member this.``should interpret int`` () =
        let tokens = lex "5"
        let parsed = parse tokens
        let result = parsed |> List.head |> evalExpression
        result |> should equal (IntResult(5))

    [<Test>]
    member this.``should interpret float`` () =
        let tokens = lex "5.8"
        let parsed = parse tokens
        let result = parsed |> List.head |> evalExpression
        result |> should equal (FloatResult(5.8))

    [<Test>]
    member this.``should interpret list`` () =
        let tokens = lex "(5.8 1.2 6)"
        let parsed = parse tokens
        let result = parsed |> List.head |> evalExpression
        result |> should equal (ListResult [FloatResult(5.8); FloatResult(1.2); IntResult(6)])

    [<Test>]
    member this.``should interpret nested lists`` () =
        let tokens = lex "((5 1) (6))"
        let parsed = parse tokens
        let result = parsed |> List.head |> evalExpression
        result |> should equal (ListResult [
            ListResult [IntResult 5; IntResult 1;];
            ListResult [IntResult 6]])

    [<Test>]
    member this.``parse error gives error result`` () =
        let tokens = lex "(1 2"
        let parsed = parse tokens
        let result = parsed |> List.head |> evalExpression
        result |> should equal (ErrorResult "Expected ')'")

