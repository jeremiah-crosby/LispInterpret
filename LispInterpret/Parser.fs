module Parser

open Lexer
open Syntax

type private ParseState = {
    Expressions: Expression list
    Remaining: Token list
}

let private error (message: string): ParseState =
    { Expressions = [ ErrorExpr message ]; Remaining = [] }

let rec private parseExpression (state: ParseState): ParseState =
    match state.Remaining with
    | [] -> state
    | Error msg :: _ -> error msg
    | RightParenthesis :: _-> error "unmatched ')'"
    | LeftParenthesis :: rest ->
        let list = parseList({state with Remaining = rest})
        match list.Remaining with
        | RightParenthesis :: remaining -> {list with Remaining = remaining}
        | _ -> error "Expected ')'"
    | Number(Some(intNum), None) :: rest -> {Expressions = state.Expressions @ [IntExpr(intNum)]; Remaining = rest}
    | Number(None, Some(floatNum)) :: rest -> {Expressions = state.Expressions @ [FloatExpr(floatNum)]; Remaining = rest}
    | Number(Some _, Some _):: _ -> error "Number cannot be both float and int"
    | Number(None, None) :: _ -> error "Number must be an int or float"
    | LiteralString s :: rest -> {Expressions = state.Expressions @ [StringExpr s]; Remaining = rest}
    | Symbol(name) :: rest -> {Expressions = state.Expressions @ [SymbolExpr name]; Remaining = rest}
and private parseList (state: ParseState) =
    let members = parseListMembers({state with Expressions = []})
    {Expressions = state.Expressions @ [ListExpr members.Expressions]; Remaining = members.Remaining}
and private parseListMembers (state: ParseState) =
    match state.Remaining with
    | RightParenthesis :: _ -> state
    | [] -> state
    | _ -> state |> parseExpression |> parseListMembers

let rec private parseExpressions (state: ParseState): ParseState =
    let parsed = parseExpression state
    match parsed.Remaining with
    | [] -> parsed
    | _ -> parseExpressions parsed

let parse (tokens: Token list): Expression list =
    let parsed = parseExpressions {Expressions = []; Remaining = tokens}
    parsed.Expressions