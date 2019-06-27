namespace Tests

open NUnit.Framework
open FsUnit
open Interpreter

type InterpreterTests () =

    [<Test>]
    member this.``should interpret int`` () =
        let (result, _) = TestHelpers.evalString "5" 
        result |> should equal (IntResult(5))

    [<Test>]
    member this.``should interpret float`` () =
        let (result, _) = TestHelpers.evalString "5.8"
        result |> should equal (FloatResult(5.8))

    [<Test>]
    member this.``should interpret string`` () =
        let (result, _) = TestHelpers.evalString "\"my string\""
        result |> should equal (StringResult("my string"))

    [<Test>]
    member this.``should interpret list`` () =
        let (result, _) = TestHelpers.evalString "(5.8 1.2 6)"
        result |> should equal (ListResult [FloatResult(5.8); FloatResult(1.2); IntResult(6)])

    [<Test>]
    member this.``should interpret nested lists`` () =
        let (result, _) = TestHelpers.evalString "((5 1) (6))"
        result |> should equal (ListResult [
            ListResult [IntResult 5; IntResult 1;];
            ListResult [IntResult 6]])

    [<Test>]
    member this.``parse error gives error result`` () =
        let (result, _) = TestHelpers.evalString "(1 2"
        result |> should equal (ErrorResult "Expected ')'")

    [<Test>]
    member this.``should retrieve variable from environment`` () =
        let (result, _) = TestHelpers.evalString @"
            (set x 5)
            x
        "
        result |> should equal (IntResult 5)