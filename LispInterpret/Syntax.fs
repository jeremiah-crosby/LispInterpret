module Syntax

type ArgumentExpr = {
    TypeName: string
    ArgumentName: string
}

type Expression =
    | IntExpr of int
    | FloatExpr of float
    | DefunExpr of name: string * arguments: ArgumentExpr list * body: Expression
    | InvokeExpr of name: string * arguments: Expression list
    | ErrorExpr of string
    | NilExpr
