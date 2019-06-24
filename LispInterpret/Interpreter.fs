module Interpreter

open Syntax

type EvalResult =
    | IntResult of int
    | FloatResult of float
    | Empty

//type Environment = {
    
//}

let evalExpression (expr: Expression) =
    match expr with
    | IntExpr(n) -> IntResult n
    | FloatExpr(d) -> FloatResult d
    | _ -> Empty
