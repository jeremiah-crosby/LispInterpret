namespace Tests

open NUnit.Framework
open FsUnit
open Syntax

type CdrTests () =
    
    [<Test>]
    member this.``(cdr (list 1 2)) = (2)`` () =
        let result = TestHelpers.evalString @"(cdr (list 1 2))"
        result |> should equal (ListExpr [IntExpr 2])

    [<Test>]
    member this.``(cdr (list)) = ()`` () =
        let result = TestHelpers.evalString @"(cdr (list))"
        result |> should equal (ListExpr [])

    [<Test>]
    member this.``(cdr 1) returns invalid arguments`` () =
        let result = TestHelpers.evalString @"(cdr 1)"
        result |> should equal (ErrorExpr "Invalid arguments")

    [<Test>]
    member this.``(cdr (list 1 2) "some other arg") returns invalid arguments`` () =
        let result = TestHelpers.evalString @"(cdr (list 1 2) ""some other arg"")"
        result |> should equal (ErrorExpr "Invalid arguments")

    [<Test>]
    member this.``(car) returns invalid arguments`` () =
        let result = TestHelpers.evalString @"(cdr)"
        result |> should equal (ErrorExpr "Invalid arguments")