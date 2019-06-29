module Interpreter

open Syntax

type DefunArgument = {
    Name: string
}

type DefunData = {
    Name: string
    Arguments: DefunArgument list
    Body: Expression list
}

type EvalResult =
    | IntResult of int
    | FloatResult of float
    | StringResult of string
    | ListResult of EvalResult list
    | DefunResult of DefunData
    | Empty
    | ErrorResult of string

type Environment = {
    ParentEnv: Option<Environment>
    Variables: Map<string, EvalResult>
}

exception EvaluationError of string

let addOrUpdateBinding (environment: Environment) (symbol: string) (value: EvalResult) =
    {environment with Variables = Map.add symbol value environment.Variables}

let rec retrieveBinding (environment: Environment) (symbol: string) =
    match Map.tryFind symbol environment.Variables with
    | None ->
        match environment.ParentEnv with
        | None -> failwith "Variable not found"
        | Some(parentEnv) -> retrieveBinding parentEnv symbol
    | Some(binding) -> binding

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
    | ListExpr(SymbolExpr "defun" :: SymbolExpr name :: ListExpr argList :: body) -> evalDefun name argList body environment
    | ListExpr(SymbolExpr "+" :: rest) -> evalAdd rest environment
    | ListExpr([SymbolExpr "set"; SymbolExpr setSymbol; value]) -> evalSet setSymbol value environment
    | ListExpr(SymbolExpr f :: rest) -> evalInvoke f rest environment
    | ListExpr(list) -> evalList list environment
    | _ -> (Empty, environment)
and evalExpressions (expressions: Expression list) (environment: Environment) = 
    let evaluator (_: EvalResult, env: Environment) (expr: Expression) =
        let result = evalExpression expr env
        match result with
        | (ErrorResult(msg), _) ->  raise (EvaluationError(msg))
        | _ -> result
    try
        List.fold evaluator (Empty, environment) expressions
    with
        | EvaluationError(msg) -> (ErrorResult(msg), environment)
and evalInvoke (name: string) (parameters: Expression list) (environment: Environment) =
    let getArgNames (args: DefunArgument list) =
        List.map (fun (a: DefunArgument) -> a.Name) args
    match retrieveBinding environment name with
    | DefunResult({Name=name; Arguments=args; Body=body}) ->
        let funcEnvironment = {
            Variables = parameters |> List.map (fun p ->
                                                    let (r, _) = evalExpression p environment
                                                    r)
                                   |> List.zip (getArgNames args)
                                   |> Map.ofList;
            ParentEnv = Some(environment)
        }
        evalExpressions body funcEnvironment
    | _ -> (ErrorResult("Is not a function"), environment)
and evalDefun (name: string) (argList: Expression list) (body: Expression list) (environment: Environment) =
    try
        let result = DefunResult({
            Name = name
            Arguments = List.map (fun a ->
                                    match a with
                                    | SymbolExpr n -> {Name = n}
                                    | _ -> raise (EvaluationError("Arguments to defun must be symbols")))
                                argList
            Body = body
        })
        (result, addOrUpdateBinding environment name result )
    with
        | EvaluationError(msg) -> (ErrorResult(msg), environment)
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
