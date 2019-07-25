namespace Tests

open NUnit.Framework
open FsUnit
open Syntax

type CarTests () =
    
    [<Test>]
    member this.``(car (list 1 2)) = 1`` () =
        let result = TestHelpers.evalString @"(car (list 1 2))"
        result |> should equal (IntExpr 1)

    [<Test>]
    member this.``(car 1) returns invalid arguments`` () =
        let result = TestHelpers.evalString @"(car 1)"
        result |> should equal (ErrorExpr "Invalid arguments")

    [<Test>]
    member this.``(car (list 1 2) "some other arg") returns invalid arguments`` () =
        let result = TestHelpers.evalString @"(car (list 1 2) ""some other arg"")"
        result |> should equal (ErrorExpr "Invalid arguments")

    [<Test>]
    member this.``(car) returns invalid arguments`` () =
        let result = TestHelpers.evalString @"(car)"
        result |> should equal (ErrorExpr "Invalid arguments")