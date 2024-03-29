﻿module Interpreter

open Syntax

exception EvaluationError of string

let rec retrieveBindingValue (environment: Environment) (symbol: string) =
    match Map.tryFind symbol environment.Variables.Value with
    | None ->
        match environment.ParentEnv with
        | None -> ErrorExpr "Variable not found"
        | Some(parentEnv) -> retrieveBindingValue parentEnv symbol
    | Some(binding) -> binding.Value

let rec retrieveBindingRef (environment: Environment) (symbol: string) =
    match Map.tryFind symbol environment.Variables.Value with
    | None ->
        match environment.ParentEnv with
        | None -> None
        | Some(parentEnv) -> retrieveBindingRef parentEnv symbol
    | _ as binding -> binding

let addOrUpdateBinding (environment: Environment) (symbol: string) (value: Expression) =
    let existingBinding = retrieveBindingRef environment symbol
    match existingBinding with
    | None _ ->
        environment.Variables := environment.Variables.Value.Add(symbol, ref (value))
        NilExpr
    | Some(binding) ->
        binding := value
        NilExpr

let rec retrieveIntrinsic (environment: Environment) (symbol: string) =
    match Map.tryFind symbol environment.Intrinsics.Value with
    | None ->
        match environment.ParentEnv with
        | None -> failwith "Variable not found"
        | Some(parentEnv) -> retrieveIntrinsic parentEnv symbol
    | Some(binding) -> binding

let coerceFloat = function
| IntExpr(n) -> (float n)
| FloatExpr(f) -> f
| _ -> raise (EvaluationError("Not a number"))

let mathOpList list op =
    try
        let anyFloat = List.exists (fun e -> match e with | FloatExpr(_) -> true | _ -> false) list
        let mathResult = list |> List.map (fun result -> coerceFloat result) |> List.reduce (fun x y -> op x y)
        match mathResult with
        | r when anyFloat = false -> IntExpr (int r)
        | r when anyFloat = true -> FloatExpr r
        | _ -> failwith "Not int or float"
    with
    | :? EvaluationError -> ErrorExpr("All arguments must be numeric")

let rec evalExpression (environment: Environment) (expr: Expression)  =
    match expr with
    | ErrorExpr msg -> ErrorExpr msg
    | IntExpr(n) -> IntExpr n
    | FloatExpr(d) -> FloatExpr d
    | StringExpr(s) -> StringExpr s
    | SymbolExpr "nil" -> NilExpr
    | SymbolExpr(s) -> retrieveBindingValue environment s
    | ListExpr(SymbolExpr "defun" :: SymbolExpr name :: ListExpr argList :: body) -> evalDefun name argList body environment
    | ListExpr([SymbolExpr "set"; SymbolExpr setSymbol; value]) -> evalSet setSymbol value environment
    | ListExpr([SymbolExpr "quote"; _ as expr]) -> expr
    | ListExpr(SymbolExpr "if" :: test :: rest) -> evalIf environment test rest
    | ListExpr(SymbolExpr "lambda" :: ListExpr parameters :: body) -> evalLambda environment parameters body
    | ListExpr(SymbolExpr f :: rest) -> evalInvoke f rest environment
    | ListExpr(list) -> evalList list environment
    | _ -> NilExpr
and evalIf (environment: Environment) (test: Expression) (body: Expression list) =
    let len = List.length body
    if len < 1 then
        ErrorExpr "At least 1 body expression required for if"
    else if len > 2 then
        ErrorExpr "Too many body expressions for if, expecting 1 or 2"
    else
        match evalExpression environment test with
        | NilExpr | ListExpr([]) ->
            if List.length body = 1 then
                NilExpr
            else
                body |> List.item 1 |> evalExpression environment
        | _ -> body |> List.head |> evalExpression environment
and evalExpressions (environment: Environment) (expressions: Expression list) = 
    let evaluator (_: Expression) (expr: Expression) =
        let result = evalExpression environment expr
        match result with
        | ErrorExpr(msg) ->  raise (EvaluationError(msg))
        | _ -> result
    try
        List.fold evaluator NilExpr expressions
    with
        | EvaluationError(msg) -> ErrorExpr(msg)
and mapEval (env: Environment) (expressions: Expression list) =
    List.map (fun exp -> evalExpression env exp) expressions
and evalInvoke (name: string) (parameters: Expression list) (environment: Environment) =
    let getArgNames (args: FunctionArgument list) =
        List.map (fun (a: FunctionArgument) -> a.Name) args
    match retrieveBindingValue environment name with
    | FunctionExpr({Name=name; Arguments=args; Body=body; Environment=storedEnv}) ->
        let funcEnvironment = {
            Variables = ref (parameters |> mapEval environment
                                   |> List.map ref
                                   |> List.zip (getArgNames args)
                                   |> Map.ofList);
            ParentEnv = Some(storedEnv);
            Intrinsics = ref (Map.empty)
        }
        let result = evalExpressions funcEnvironment body
        result
    | _ -> 
        let func = retrieveIntrinsic environment name
        func (mapEval environment parameters) environment
        
and evalDefun (name: string) (argList: Expression list) (body: Expression list) (environment: Environment) =
    try
        let result = FunctionExpr({
            Name = name
            Arguments = List.map (fun a ->
                                    match a with
                                    | SymbolExpr n -> {Name = n}
                                    | _ -> raise (EvaluationError("Arguments to defun must be symbols")))
                                argList
            Body = body
            Environment = {
                Variables = ref (Map.empty);
                ParentEnv = Some(environment);
                Intrinsics = ref (Map.empty)
            }
        })
        addOrUpdateBinding environment name result |> ignore
        result
    with
        | EvaluationError(msg) -> ErrorExpr(msg)
and evalLambda (environment: Environment) (parameters: Expression list) (body: Expression list) =
    try
        let result = FunctionExpr({
            Name = ""
            Arguments = List.map (fun a ->
                                    match a with
                                    | SymbolExpr n -> {Name = n}
                                    | _ -> raise (EvaluationError("Arguments to defun must be symbols")))
                                parameters
            Body = body
            Environment = {
                Variables = ref (Map.empty);
                ParentEnv = Some(environment);
                Intrinsics = ref (Map.empty)
            }
        })
        result
    with
        | EvaluationError(msg) -> ErrorExpr(msg)
and evalSet (symbol: string) (valueExpr: Expression) (environment: Environment) =
    let value = evalExpression environment valueExpr
    addOrUpdateBinding environment symbol value |> ignore
    NilExpr
and evalList (list: Expression list) (environment: Environment) =
    list |> List.map (fun (e: Expression) -> evalExpression environment e) |> ListExpr


let evalMath op (args: Expression list) (environment: Environment) =
    match args with
    | _ when List.length args >= 2 -> mathOpList args op
    | _ -> ErrorExpr "At least 2 numeric arguments required"

let evalCompare op (args: Expression list) (env: Environment) =
    match args with
    | [expr1; expr2;] ->
        if op (coerceFloat expr1) (coerceFloat expr2) then
            SymbolExpr "T"
        else
            NilExpr
    | _ -> failwith "Exactly 2 numeric arguments required"

let cons args env =
    match args with
    | [a; ListExpr(b)] -> ListExpr([a] @ b)
    | [a; NilExpr] -> ListExpr([a])
    | [a; b] -> ListExpr [a; b]
    | _ -> ErrorExpr("Exactly two arguments expected")

let car args env =
    match args with
    | [ListExpr []] -> NilExpr
    | [ListExpr(first::_)] -> first
    | _ -> ErrorExpr("Invalid arguments")

let cdr args env =
    match args with
    | [ListExpr []]-> ListExpr []
    | [ListExpr(_::rest)] -> ListExpr rest
    | _ -> ErrorExpr("Invalid arguments")

let createGlobalEnv () =
    {
        Variables = ref (Map.empty);
        ParentEnv = None;
        Intrinsics = ref ([
            ("+", evalMath (+))
            ("-", evalMath (-))
            ("/", evalMath (/))
            ("*", evalMath (*))
            ("<", evalCompare (<))
            (">", evalCompare (>))
            (">=", evalCompare (>=))
            ("<=", evalCompare (<=))
            ("=", evalCompare (=))
            ("list", (fun args _ -> ListExpr(args)))
            ("cons", cons)
            ("car", car)
            ("cdr", cdr)
        ] |> Map.ofList)
    }

let rec printExpression = function
| (ListExpr(list), env) -> "(" + System.String.Join(" ", (List.map (fun e -> printExpression (e, env)) list)) + ")"
| (StringExpr(s), _) ->
    let escape = String.collect (function '"' -> "\\\"" | c -> c.ToString()) // escape quotes
    "\"" + (escape s) + "\""
| (SymbolExpr(s), _) -> s
| (IntExpr(n), _) -> n.ToString()
| (FloatExpr(f), _) -> f.ToString()
| (FunctionExpr(_), _) -> "Function"
| (NilExpr, _) -> "nil"
| (ErrorExpr(e), _) -> String.concat " " ["Error:"; e]
| (BoolExpr false, _) -> "false"
| (BoolExpr true, _) -> "true"