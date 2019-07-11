namespace Tests

open NUnit.Framework
open FsUnit
open Syntax

type RelationalOperatorTests () =

    [<Test>]
    member this.``(< 1 2) = T`` () =
        let (result, _) = TestHelpers.evalString "(< 1 2)"
        result |> should equal (SymbolExpr "T")

    [<Test>]
    member this.``(< 2 1) = nil`` () =
        let (result, _) = TestHelpers.evalString "(< 2 1)"
        result |> should equal (NilExpr)

    [<Test>]
    member this.``(< 1 1.32403) = T`` () =
        let (result, _) = TestHelpers.evalString "(< 1 1.32403)"
        result |> should equal (SymbolExpr "T")
            
    [<Test>]
    member this.``(<= 6 6) = T`` () =
        let (result, _) = TestHelpers.evalString "(<= 6 6)"
        result |> should equal (SymbolExpr "T")

    [<Test>]
    member this.``(> 6 5) = T`` () =
        let (result, _) = TestHelpers.evalString "(> 6 5)"
        result |> should equal (SymbolExpr "T")

    [<Test>]
    member this.``(>= 6 6) = T`` () =
        let (result, _) = TestHelpers.evalString "(>= 6 6)"
        result |> should equal (SymbolExpr "T")

    [<Test>]
    member this.``(= 6 6) = T`` () =
        let (result, _) = TestHelpers.evalString "(= 6 6)"
        result |> should equal (SymbolExpr "T")

    [<Test>]
    member this.``(= 7 6) = nil`` () =
        let (result, _) = TestHelpers.evalString "(= 7 6)"
        result |> should equal (NilExpr)



