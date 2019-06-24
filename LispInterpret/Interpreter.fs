module Interpreter

open Syntax

type EvalResult =
    | IntResult of int
    | FloatResult of float
    | ListResult of EvalResult list
    | Empty

//type Environment = {
//    ParentEnv: Environment
//    Variables: Map<string, EvalResult>
//}

let rec evalExpression (expr: Expression) =
    match expr with
    | IntExpr(n) -> IntResult n
    | FloatExpr(d) -> FloatResult d
    | ListExpr(list) ->
        list |> List.map (fun (e: Expression) -> evalExpression(e)) |> ListResult
    | _ -> Empty
