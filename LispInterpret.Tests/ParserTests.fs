namespace Tests

open NUnit.Framework
open FsUnit
open Syntax
open Parser

[<TestClass>]
type ParserTests () =
    
    [<Test>]
    member this.``should parse empty`` () =
        let tokens = []
        parse tokens |> should equal []