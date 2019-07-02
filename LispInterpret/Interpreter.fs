module Interpreter

open Syntax

type Environment = {
    ParentEnv: Option<Environment>
    Variables: Map<string, Expression>
    Intrinsics: Map<string, IntrinsicFunction>
}
and IntrinsicFunction = Expression list -> Environment -> Expression * Environment

exception EvaluationError of string

let addOrUpdateBinding (environment: Environment) (symbol: string) (value: Expression) =
    {environment with Variables = Map.add symbol value environment.Variables}

let rec retrieveBinding (environment: Environment) (symbol: string) =
    match Map.tryFind symbol environment.Variables with
    | None ->
        match environment.ParentEnv with
        | None -> failwith "Variable not found"
        | Some(parentEnv) -> retrieveBinding parentEnv symbol
    | Some(binding) -> binding

let rec retrieveIntrinsic (environment: Environment) (symbol: string) =
    match Map.tryFind symbol environment.Intrinsics with
    | None ->
        match environment.ParentEnv with
        | None -> failwith "Variable not found"
        | Some(parentEnv) -> retrieveIntrinsic parentEnv symbol
    | Some(binding) -> binding

let addList list =
    try
        if (List.exists (fun e -> match e with | FloatExpr(_) -> true | _ -> false) list) then
            let sum = list |> List.map (fun result ->
                match result with
                | FloatExpr x -> x
                | IntExpr i -> (float i)
                | _ -> invalidArg "result" "All arguments must be numeric") |> List.sum
            FloatExpr sum
        else
            let sum = list |> List.map (fun result ->
                match result with
                | IntExpr x -> x
                | _ -> invalidArg "result" "All arguments must be numeric") |> List.sum
            IntExpr sum
    with
    | :? System.ArgumentException -> ErrorExpr("All arguments must be numeric")

let rec evalExpression (expr: Expression) (environment: Environment) =
    match expr with
    | ErrorExpr msg -> (ErrorExpr msg, environment)
    | IntExpr(n) -> (IntExpr n, environment)
    | FloatExpr(d) -> (FloatExpr d, environment)
    | StringExpr(s) -> (StringExpr s, environment)
    | SymbolExpr(s) -> (retrieveBinding environment s, environment)
    | ListExpr(SymbolExpr "defun" :: SymbolExpr name :: ListExpr argList :: body) -> evalDefun name argList body environment
    | ListExpr([SymbolExpr "set"; SymbolExpr setSymbol; value]) -> evalSet setSymbol value environment
    | ListExpr([SymbolExpr "quote"; _ as expr]) -> (expr, environment)
    | ListExpr(SymbolExpr f :: rest) -> evalInvoke f rest environment
    | ListExpr(list) -> evalList list environment
    | _ -> (NilExpr, environment)
and evalExpressions (expressions: Expression list) (environment: Environment) = 
    let evaluator (_: Expression, env: Environment) (expr: Expression) =
        let result = evalExpression expr env
        match result with
        | (ErrorExpr(msg), _) ->  raise (EvaluationError(msg))
        | _ -> result
    try
        List.fold evaluator (NilExpr, environment) expressions
    with
        | EvaluationError(msg) -> (ErrorExpr(msg), environment)
and evalInvoke (name: string) (parameters: Expression list) (environment: Environment) =
    let getArgNames (args: DefunArgument list) =
        List.map (fun (a: DefunArgument) -> a.Name) args
    try
        match retrieveBinding environment name with
        | DefunExpr({Name=name; Arguments=args; Body=body}) ->
            let funcEnvironment = {
                Variables = parameters |> List.map (fun p ->
                                                        let (r, _) = evalExpression p environment
                                                        r)
                                       |> List.zip (getArgNames args)
                                       |> Map.ofList;
                ParentEnv = Some(environment);
                Intrinsics = Map.empty
            }
            evalExpressions body funcEnvironment
        | _ -> (ErrorExpr("Is not a function"), environment)
    with
    | Failure(msg) ->
        match retrieveIntrinsic environment name with
        | IntrinsicFunction as func -> func parameters environment
        | _ -> (ErrorExpr("Is not a function"), environment)
and evalDefun (name: string) (argList: Expression list) (body: Expression list) (environment: Environment) =
    try
        let result = DefunExpr({
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
        | EvaluationError(msg) -> (ErrorExpr(msg), environment)
and evalSet (symbol: string) (valueExpr: Expression) (environment: Environment) =
    let (value, newEnv) = evalExpression valueExpr environment
    (NilExpr, addOrUpdateBinding newEnv symbol value)
and evalList (list: Expression list) (environment: Environment) =
    (list |> List.map (fun (e: Expression) -> evalExpression e environment) |> List.map (fun (result, _) -> result) |> ListExpr, environment)
and evalAdd (args: Expression list) (environment: Environment) =
    let evaluated = evalList args environment
    match evaluated with
    | (ListExpr([ListExpr addends]), updatedEnv) -> (addList addends, updatedEnv)
    | (ListExpr list, updatedEnv) when List.length list >= 2 -> (addList list, updatedEnv)
    | _ -> (ErrorExpr "At least 2 numeric arguments required", environment)

let createGlobalEnv () =
    {
        Variables = Map.empty;
        ParentEnv = None;
        Intrinsics = [
            ("+", evalAdd)
        ] |> Map.ofList
    }