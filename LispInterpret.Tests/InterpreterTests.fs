namespace Tests

open NUnit.Framework
open FsUnit
open Syntax
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

    [<Test>]
    member this.``should generate function definition for defun with empty arg list`` () =
        let (result, env) = TestHelpers.evalString "(defun x () (+ 1 2))"
        result |> should equal (DefunResult{ Name = "x"; Arguments = []; Body = [ListExpr [SymbolExpr "+"; IntExpr 1; IntExpr 2]]})
        env.Variables |> Map.containsKey "x" |> should equal true

    [<Test>]
    member this.``should generate function definition for defun with non-empty arg list`` () =
        let (result, env) = TestHelpers.evalString "(defun x (y z) (+ y z))"
        result |> should equal (DefunResult{ Name = "x"; Arguments = [{Name = "y";}; {Name = "z";}]; Body = [ListExpr [SymbolExpr "+"; SymbolExpr "y"; SymbolExpr "z"]]})
        env.Variables |> Map.containsKey "x" |> should equal true

    [<Test>]
    member this.``defun with args that are not a list of symbols should error`` () =
        let (result, _) = TestHelpers.evalString "(defun x (5) ())"
        result |> should equal (ErrorResult "Arguments to defun must be symbols")