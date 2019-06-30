module Syntax

type ArgumentExpr = {
    TypeName: string
    ArgumentName: string
}

type DefunArgument = {
    Name: string
}

type Expression =
    | IntExpr of int
    | FloatExpr of float
    | SymbolExpr of name: string
    | ErrorExpr of string
    | ListExpr of Expression list
    | StringExpr of string
    | DefunExpr of DefunData
    | NilExpr
and DefunData = {
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
