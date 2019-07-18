namespace Tests

open NUnit.Framework
open FsUnit
open Syntax

type ConsTests () =
    
    [<Test>]
    member this.``(cons 1 2) = (1 2)`` () =
        let (result, _) = TestHelpers.evalString @"(cons 1 2)"
        result |> should equal (ListExpr([IntExpr(1); IntExpr(2)]))

    [<Test>]
    member this.``(cons 1 (list 2 3)) = (1 2 3)`` () =
        let (result, _) = TestHelpers.evalString @"(cons 1 (list 2 3))"
        result |> should equal (ListExpr([IntExpr(1); IntExpr(2); IntExpr(3)]))

    [<Test>]
    member this.``(cons 1 (cons 2 (cons 3 nil))) = (1 2 3)`` () =
        let (result, _) = TestHelpers.evalString @"(cons 1 (cons 2 (cons 3 nil)))"
        result |> should equal (ListExpr([IntExpr(1); IntExpr(2); IntExpr(3)]))

    [<Test>]
    member this.``(cons 1) = Error`` () =
        let (result, _) = TestHelpers.evalString @"(cons 1)"
        result |> should equal (ErrorExpr "Exactly two arguments expected")

    [<Test>]
    member this.``(cons 1 2 3) = Error`` () =
        let (result, _) = TestHelpers.evalString @"(cons 1 2 3)"
        result |> should equal (ErrorExpr "Exactly two arguments expected")