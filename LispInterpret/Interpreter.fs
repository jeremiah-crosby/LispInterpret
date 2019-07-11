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
        | None -> ErrorExpr "Variable not found"
        | Some(parentEnv) -> retrieveBinding parentEnv symbol
    | Some(binding) -> binding

let rec retrieveIntrinsic (environment: Environment) (symbol: string) =
    match Map.tryFind symbol environment.Intrinsics with
    | None ->
        match environment.ParentEnv with
        | None -> failwith "Variable not found"
        | Some(parentEnv) -> retrieveIntrinsic parentEnv symbol
    | Some(binding) -> binding

let mathOpList list op =
    try
        (*
            To deal with mixed float and integer math operations, the operations are all performed on floats. We
            keep track of a boolean saying whether all operands are ints (the second part of the tuple created in map).
            The reduce performing the operation accumulates whether all operands are ints. If so, it casts the result to
            an int, other it will be a float.
        *)
        let mathResult = list |> List.map (fun result ->
            match result with
            | FloatExpr x -> x, false
            | IntExpr i -> (float i), true
            | _ -> invalidArg "result" "All arguments must be numeric") |> List.reduce (fun (x, xint) (y, yint) -> op x y, xint && yint)
        match mathResult with
        | (r, true) -> IntExpr (int r)
        | (r, false) -> FloatExpr r
    with
    | :? System.ArgumentException -> ErrorExpr("All arguments must be numeric")

let compare op (args: Expression list) (env: Environment) =
    match args with
    | [expr1; expr2;] ->
        let f1 = match expr1 with
            | IntExpr(i) -> (float i)
            | FloatExpr(f) -> f
            | _ -> failwith "Arguments must be numeric"
        let f2 = match expr2 with
            | IntExpr(i) -> (float i)
            | FloatExpr(f) -> f
            | _ -> failwith "Arguments must be numeric"
        if op f1 f2 then
            SymbolExpr "T", env
        else
            NilExpr, env
    | _ -> failwith "Exactly 2 numeric arguments required"

let rec evalExpression (environment: Environment) (expr: Expression)  =
    match expr with
    | ErrorExpr msg -> (ErrorExpr msg, environment)
    | IntExpr(n) -> (IntExpr n, environment)
    | FloatExpr(d) -> (FloatExpr d, environment)
    | StringExpr(s) -> (StringExpr s, environment)
    | SymbolExpr "nil" -> (NilExpr, environment)
    | SymbolExpr(s) -> (retrieveBinding environment s, environment)
    | ListExpr(SymbolExpr "defun" :: SymbolExpr name :: ListExpr argList :: body) -> evalDefun name argList body environment
    | ListExpr([SymbolExpr "set"; SymbolExpr setSymbol; value]) -> evalSet setSymbol value environment
    | ListExpr([SymbolExpr "quote"; _ as expr]) -> (expr, environment)
    | ListExpr(SymbolExpr "if" :: test :: rest) -> evalIf environment test rest
    | ListExpr(SymbolExpr f :: rest) -> evalInvoke f rest environment
    | ListExpr(list) -> evalList list environment
    | _ -> (NilExpr, environment)
and evalIf (environment: Environment) (test: Expression) (body: Expression list) =
    let len = List.length body
    if len < 1 then
        ErrorExpr "At least 1 body expression required for if", environment
    else if len > 2 then
        ErrorExpr "Too many body expressions for if, expecting 1 or 2", environment
    else
        match evalExpression environment test with
        | (NilExpr, _) | (ListExpr([]), _) ->
            if List.length body = 1 then
                NilExpr, environment
            else
                body |> List.item 1 |> evalExpression environment
        | _ -> body |> List.head |> evalExpression environment
and evalExpressions (environment: Environment) (expressions: Expression list) = 
    let evaluator (_: Expression, env: Environment) (expr: Expression) =
        let result = evalExpression env expr
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
    match retrieveBinding environment name with
    | DefunExpr({Name=name; Arguments=args; Body=body}) ->
        let funcEnvironment = {
            Variables = parameters |> List.map (fun p ->
                                                    let (r, _) = evalExpression environment p
                                                    r)
                                   |> List.zip (getArgNames args)
                                   |> Map.ofList;
            ParentEnv = Some(environment);
            Intrinsics = Map.empty
        }
        let result, _ = evalExpressions funcEnvironment body
        result, environment
    | _ -> 
        match retrieveIntrinsic environment name with
        | IntrinsicFunction as func ->
            let result, _ = func parameters environment
            result, environment
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
    let (value, newEnv) = evalExpression environment valueExpr
    (NilExpr, addOrUpdateBinding newEnv symbol value)
and evalList (list: Expression list) (environment: Environment) =
    (list |> List.map (fun (e: Expression) -> evalExpression environment e) |> List.map (fun (result, _) -> result) |> ListExpr, environment)
and evalMath op (args: Expression list) (environment: Environment) =
    let evaluated = evalList args environment
    match evaluated with
    | (ListExpr list, updatedEnv) when List.length list >= 2 -> (mathOpList list op, updatedEnv)
    | _ -> (ErrorExpr "At least 2 numeric arguments required", environment)

let createGlobalEnv () =
    {
        Variables = Map.empty;
        ParentEnv = None;
        Intrinsics = [
            ("+", evalMath (+))
            ("-", evalMath (-))
            ("/", evalMath (/))
            ("*", evalMath (*))
            ("<", compare (<))
            (">", compare (>))
            (">=", compare (>=))
            ("<=", compare (<=))
            ("=", compare (=))
        ] |> Map.ofList
    }

let rec printExpression = function
| (ListExpr(list), env) -> "(" + System.String.Join(" ", (List.map (fun e -> printExpression (e, env)) list)) + ")"
| (StringExpr(s), _) ->
    let escape = String.collect (function '"' -> "\\\"" | c -> c.ToString()) // escape quotes
    "\"" + (escape s) + "\""
| (SymbolExpr(s), _) -> s
| (IntExpr(n), _) -> n.ToString()
| (FloatExpr(f), _) -> f.ToString()
| (DefunExpr(_), _) -> "Function"
| (NilExpr, _) -> "nil"
| (ErrorExpr(e), _) -> String.concat " " ["Error"; e]