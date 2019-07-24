namespace Tests

open NUnit.Framework
open FsUnit
open Syntax

type QuoteTests () =
    
    [<Test>]
    member this.``(quote x) should return symbol x`` () =
        let result = TestHelpers.evalString "(quote x)"
        result |> should equal (SymbolExpr "x")

    [<Test>]
    member this.``(quote (1 2 3)) should return (1 2 3)`` () =
        let result = TestHelpers.evalString "(quote (1 2 3))"
        result |> should equal (ListExpr([IntExpr 1; IntExpr 2; IntExpr 3]))

