module Syntax

open System

type ArgumentExpr = {
    TypeName: string
    ArgumentName: string
}

type FunctionArgument = {
    Name: string
}

type Expression =
    | IntExpr of int
    | FloatExpr of float
    | SymbolExpr of name: string
    | ErrorExpr of string
    | ListExpr of Expression list
    | StringExpr of string
    | FunctionExpr of FunctionData
    | BoolExpr of bool
    | NilExpr
and FunctionData = {
    Name: string
    Arguments: FunctionArgument list
    Body: Expression list
    Environment: Environment
}
and Environment = {
    ParentEnv: Option<Environment>
    Variables: Map<string, Expression ref> ref
    Intrinsics: Map<string, IntrinsicFunction> ref
}
and IntrinsicFunction = Expression list -> Environment -> Expression * Environment
