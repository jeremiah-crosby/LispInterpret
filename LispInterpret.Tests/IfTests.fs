namespace Tests

open NUnit.Framework
open FsUnit
open Syntax

type IfTests () =

    [<Test>]
    member this.``should evaluate first expression when true`` () =
        let (result, _) = TestHelpers.evalString "(if t \"true\" \"false\")"
        result |> should equal (StringExpr "true")

    [<Test>]
    member this.``should evaluate second expression when nil`` () =
        let (result, _) = TestHelpers.evalString "(if nil \"true\" \"false\")"
        result |> should equal (StringExpr "false")

    [<Test>]
    member this.``test single body expression for t`` () =
        let (result, _) = TestHelpers.evalString "(if t \"true\")"
        result |> should equal (StringExpr "true")

    [<Test>]
    member this.``test single body expression for nil`` () =
        let (result, _) = TestHelpers.evalString "(if nil \"true\")"
        result |> should equal (NilExpr)

    [<Test>]
    member this.``empty list should evaluate to nil`` () =
        let (result, _) = TestHelpers.evalString "(if () \"true\")"
        result |> should equal (NilExpr)

    [<Test>]
    member this.``no body expressions should be error`` () =
        let (result, _) = TestHelpers.evalString "(if ())"
        result |> should equal (ErrorExpr "At least 1 body expression required for if")

    [<Test>]
    member this.``too many body expressions should be error`` () =
        let (result, _) = TestHelpers.evalString "(if () 1 2 3 4)"
        result |> should equal (ErrorExpr "Too many body expressions for if, expecting 1 or 2")