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