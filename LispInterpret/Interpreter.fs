module Interpreter

open Syntax

type EvalResult =
    | IntResult of int
    | FloatResult of float
    | StringResult of string
    | ListResult of EvalResult list
    | Empty
    | ErrorResult of string

type Environment = {
    ParentEnv: Option<Environment>
    Variables: Map<string, EvalResult>
}

let addOrUpdateBinding (environment: Environment) (symbol: string) (value: EvalResult) =
    {environment with Variables = Map.add symbol value environment.Variables}

let retrieveBinding (environment: Environment) (symbol: string) =
    Map.find symbol environment.Variables

let addList list =
    try
        if (List.exists (fun e -> match e with | FloatResult(_) -> true | _ -> false) list) then
            let sum = list |> List.map (fun result ->
                match result with
                | FloatResult x -> x
                | IntResult i -> (float i)
                | _ -> invalidArg "result" "All arguments must be numeric") |> List.sum
            FloatResult sum
        else
            let sum = list |> List.map (fun result ->
                match result with
                | IntResult x -> x
                | _ -> invalidArg "result" "All arguments must be numeric") |> List.sum
            IntResult sum
    with
    | :? System.ArgumentException -> ErrorResult("All arguments must be numeric")

let rec evalExpression (expr: Expression) (environment: Environment) =
    match expr with
    | ErrorExpr msg -> (ErrorResult msg, environment)
    | IntExpr(n) -> (IntResult n, environment)
    | FloatExpr(d) -> (FloatResult d, environment)
    | StringExpr(s) -> (StringResult s, environment)
    | SymbolExpr(s) -> (retrieveBinding environment s, environment)
    | ListExpr(SymbolExpr "+" :: rest) -> evalAdd rest environment
    | ListExpr([SymbolExpr "set"; SymbolExpr setSymbol; value]) -> evalSet setSymbol value environment
    | ListExpr(list) -> evalList list environment
    | _ -> (Empty, environment)
and evalSet (symbol: string) (valueExpr: Expression) (environment: Environment) =
    let (value, newEnv) = evalExpression valueExpr environment
    (Empty, addOrUpdateBinding newEnv symbol value)
and evalList (list: Expression list) (environment: Environment) =
    //match results with
    //| [] -> (ListResult results, environment)
    //| first :: rest -> 
    (list |> List.map (fun (e: Expression) -> evalExpression e environment) |> List.map (fun (result, _) -> result) |> ListResult, environment)
and evalAdd (args: Expression list) (environment: Environment) =
    let evaluated = evalList args environment
    match evaluated with
    | (ListResult([ListResult addends]), updatedEnv) -> (addList addends, updatedEnv)
    | (ListResult list, updatedEnv) when List.length list >= 2 -> (addList list, updatedEnv)
    | _ -> (ErrorResult "At least 2 numeric arguments required", environment)
        
    
