namespace Tests

open NUnit.Framework
open Lexer
open FsUnit

type LexerTests () =

    [<Test>]
    member this.``should lex empty string`` () =
        lex "" |> should equal []

    [<Test>]
    member this.``should lex (+ 1 2)`` () =
        lex "(+ 1 2)" |> should equal [LeftParenthesis; Symbol "+" ; Number (Some 1, None); Number (Some 2, None); RightParenthesis]

    [<Test>]
    member this.``should lex "this is a string"`` () =
        lex "\"this is a string\"" |> should equal [LiteralString "this is a string"]

    [<Test>]
    member this.``should lex "missing quote`` () =
        lex "\"missing quote" |> should equal [LiteralString "missing quote"]
