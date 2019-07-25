module Syntax

open System

type ArgumentExpr = {
    TypeName: string
    ArgumentName: string
}

type FunctionArgument = {
    Name: string
}

[<CustomEqualityAttribute>]
[<NoComparison>]
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
    override this.Equals(obj) =
        match obj with
        | :? Expression as other ->
            match this, other with
            | IntExpr x, IntExpr y -> x = y
            | FloatExpr f1, FloatExpr f2 -> f1 = f2
            | SymbolExpr s1, SymbolExpr s2 -> s1 = s2
            | ErrorExpr e1, ErrorExpr e2 -> e1 = e2
            | ListExpr ls1, ListExpr ls2 -> ls1 = ls2
            | StringExpr s1, StringExpr s2 -> s1 = s2
            | FunctionExpr f1, FunctionExpr f2 ->
                f1.Name = f2.Name && f1.Arguments = f2.Arguments && f1.Body = f2.Body
            | BoolExpr b1, BoolExpr b2 -> b1 = b2
            | NilExpr, NilExpr -> true
            | _ -> false
        | _ -> false
and FunctionData = {
    Name: string
    Arguments: FunctionArgument list
    Body: Expression list
    Environment: Environment
}
and Environment = {
    ParentEnv: Option<Environment>
    Variables: Map<string, Expression ref> ref
    Intrinsics: Map<string, Expression list -> Environment -> Expression> ref
}

