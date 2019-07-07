namespace Tests

open NUnit.Framework
open FsUnit
open Syntax

type InterpreterTests () =

    [<Test>]
    member this.``should interpret int`` () =
        let (result, _) = TestHelpers.evalString "5" 
        result |> should equal (IntExpr(5))

    [<Test>]
    member this.``should interpret float`` () =
        let (result, _) = TestHelpers.evalString "5.8"
        result |> should equal (FloatExpr(5.8))

    [<Test>]
    member this.``should interpret string`` () =
        let (result, _) = TestHelpers.evalString "\"my string\""
        result |> should equal (StringExpr("my string"))

    [<Test>]
    member this.``should interpret list`` () =
        let (result, _) = TestHelpers.evalString "(5.8 1.2 6)"
        result |> should equal (ListExpr [FloatExpr(5.8); FloatExpr(1.2); IntExpr(6)])

    [<Test>]
    member this.``should interpret nested lists`` () =
        let (result, _) = TestHelpers.evalString "((5 1) (6))"
        result |> should equal (ListExpr [
            ListExpr [IntExpr 5; IntExpr 1;];
            ListExpr [IntExpr 6]])

    [<Test>]
    member this.``parse error gives error result`` () =
        let (result, _) = TestHelpers.evalString "(1 2"
        result |> should equal (ErrorExpr "Expected ')'")

    [<Test>]
    member this.``should retrieve variable from environment`` () =
        let (result, _) = TestHelpers.evalString @"
            (set x 5)
            x
        "
        result |> should equal (IntExpr 5)

    [<Test>]
    member this.``should generate function definition for defun with empty arg list`` () =
        let (result, env) = TestHelpers.evalString "(defun x () (+ 1 2))"
        result |> should equal (DefunExpr{ Name = "x"; Arguments = []; Body = [ListExpr [SymbolExpr "+"; IntExpr 1; IntExpr 2]]})
        env.Variables |> Map.containsKey "x" |> should equal true

    [<Test>]
    member this.``should generate function definition for defun with non-empty arg list`` () =
        let (result, env) = TestHelpers.evalString "(defun x (y z) (+ y z))"
        result |> should equal (DefunExpr{ Name = "x"; Arguments = [{Name = "y";}; {Name = "z";}]; Body = [ListExpr [SymbolExpr "+"; SymbolExpr "y"; SymbolExpr "z"]]})
        env.Variables |> Map.containsKey "x" |> should equal true

    [<Test>]
    member this.``defun with args that are not a list of symbols should error`` () =
        let (result, _) = TestHelpers.evalString "(defun x (5) ())"
        result |> should equal (ErrorExpr "Arguments to defun must be symbols")

    [<Test>]
    member this.``test invoke user defined function with no parameters`` () =
        let (result, _) = TestHelpers.evalString @"
            (defun x () 8)
            (x)
        "
        result |> should equal (IntExpr 8)

    [<Test>]
    member this.``test invoke user defined function with 1 parameter`` () =
        let (result, _) = TestHelpers.evalString @"
            (defun x (y) (+ y 8))
            (x 8)
        "
        result |> should equal (IntExpr 16)

    [<Test>]
    member this.``test setting local variable in function`` () =
        let (result, _) = TestHelpers.evalString @"
            (defun x (y)
                (set z 5)
                (+ y z))
            (x 8)
        "
        result |> should equal (IntExpr 13)

    [<Test>]
    member this.``should lookup global variables`` () =
        let (result, _) = TestHelpers.evalString @"
            (set global 1)
            (defun f2 () global)
            (defun f1 () (f2))
            (f1)
        "
        result |> should equal (IntExpr 1)

    [<Test>]
    member this.``local variables should shadow global variables`` () =
        let (result, _) = TestHelpers.evalString @"
            (set myvar 1)
            (defun f1 ()
                (set myvar 2)
                myvar)
            (f1)
        "
        result |> should equal (IntExpr 2)

    [<Test>]
    member this.``errors should short circuit evaluation`` () =
        let (result, _) = TestHelpers.evalString @"
            (set x 5)
            (set y 10)
            (+ y ""5"")
            x
            y
        "
        result |> should equal (ErrorExpr "All arguments must be numeric")

    [<Test>]
    member this.``local variables should be destroyed`` () =
        let (result, _) = TestHelpers.evalString @"
            (defun x ()
              (set y 5))
            (x)
            y
        "
        result |> should equal (ErrorExpr "Variable not found")