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
    | RightParenthesis :: _-> error "unmatched )"
    | LeftParenthesis :: RightParenthesis :: rest -> {Expressions = state.Expressions @ [NilExpr]; Remaining = rest}
    | LeftParenthesis :: Symbol name :: arguments -> parseInvoke(name, { state with Remaining = arguments })
and private parseInvoke (identifier: string, state: ParseState) =
    let argumentState = parseInvokeArguments({ state with Expressions = [] })
    match argumentState.Remaining with
    | RightParenthesis :: rest -> {Expressions = state.Expressions @ [InvokeExpr(identifier, argumentState.Expressions)]; Remaining = argumentState.Remaining}
    | [] -> error "Incomplete function call"
    | _ -> error "Expected ), found something else"
and private parseInvokeArguments (state: ParseState) =
    match state.Remaining with
    | RightParenthesis :: _ -> state
    | _ -> state |> parseExpression |> parseInvokeArguments

let rec private parseExpressions (state: ParseState): ParseState =
    let parsed = parseExpression state
    match parsed.Remaining with
    | [] -> parsed
    | _ -> parseExpressions parsed

let parse (tokens: Token list): Expression list =
    let parsed = parseExpressions {Expressions = []; Remaining = tokens}
    parsed.Expressions