module Interpreter

open Syntax

type EvalResult =
    | IntResult of int
    | FloatResult of float
    | StringResult of string
    | ListResult of EvalResult list
    | Empty
    | ErrorResult of string

//type Environment = {
//    ParentEnv: Environment
//    Variables: Map<string, EvalResult>
//}

let addList list =
    if (List.exists (fun e -> match e with | FloatResult(_) -> true | _ -> false) list) then
        let sum = list |> List.map (fun result ->
            match result with
            | FloatResult x -> x
            | IntResult i -> (float i)
            | _ -> 0.0) |> List.sum
        FloatResult sum
    else
        let sum = list |> List.map (fun result ->
            match result with
            | IntResult x -> x
            | _ -> 0) |> List.sum
        IntResult sum

let rec evalExpression (expr: Expression) =
    match expr with
    | ErrorExpr msg -> ErrorResult msg
    | IntExpr(n) -> IntResult n
    | FloatExpr(d) -> FloatResult d
    | StringExpr(s) -> StringResult s
    | ListExpr(SymbolExpr "+" :: rest) -> evalAdd rest
    | ListExpr(list) -> evalList list
    | _ -> Empty
and evalList (list: Expression list) =
    list |> List.map (fun (e: Expression) -> evalExpression(e)) |> ListResult
and evalAdd (args: Expression list) =
    let evaluated = evalList args
    match evaluated with
    | ListResult([ListResult addends]) -> addList addends
    | ListResult list when List.length list >= 2 -> addList list
    | _ -> ErrorResult "At least 2 numeric arguments required"
        
    
