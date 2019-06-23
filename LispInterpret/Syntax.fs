module Syntax

type ArgumentExpr = {
    TypeName: string
    ArgumentName: string
}

type Expression =
    | IntExpr of int
    | FloatExpr of float
    | SymbolExpr of name: string
    | ErrorExpr of string
    | ListExpr of Expression list
