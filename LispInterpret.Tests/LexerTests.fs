namespace Tests

open NUnit.Framework
open Lexer
open FsUnit

[<TestClass>]
type TestClass () =

    [<Test>]
    member this.``should lex empty string`` () =
        lex "" |> should equal []

    [<Test>]
    member this.``should lex (+ 1 2)`` () =
        lex "(+ 1 2)" |> should equal [LeftParenthesis; Symbol "+" ; Number (Some 1, None); Number (Some 2, None); RightParenthesis]
