module Lexer

type Token =
    | LeftParenthesis
    | RightParenthesis
    | Symbol of string
    | Number of int option * float option
    | LiteralString of string

let rec private lexChars (chars: char list): Token list =
    match chars with
    | '(' :: rest -> LeftParenthesis :: lexChars rest
    | ')' :: rest -> RightParenthesis :: lexChars rest
    | digit :: rest when System.Char.IsDigit digit || digit = '.' -> lexNumber(chars, "")
    | space :: rest when System.Char.IsWhiteSpace space -> lexChars rest
    | '"' :: rest -> lexString(rest, "")
    | [] -> []
    | s :: rest -> lexSymbol(chars, "")
and lexNumber (chars: char list, number: string) =
    match chars with
    | d :: rest when System.Char.IsDigit d || d = '.' -> lexNumber(rest, number + d.ToString())
    | rest -> try
                let intNum = System.Int32.Parse(number)
                Number (Some(intNum), None) :: lexChars rest
              with
              | ex -> let floatNum = System.Double.Parse(number)
                      Number(None, Some(floatNum)) :: lexChars rest
and lexSymbol (chars: char list, name: string) =
    match chars with
    | c :: rest when not (System.Char.IsWhiteSpace c) && c <> '(' && c <> ')' -> lexSymbol(rest, name + c.ToString())
    | rest -> Symbol(name) :: lexChars rest
and lexString (chars: char list, str: string) =
    match chars with
    | '"' :: rest -> LiteralString(str) :: lexChars rest
    | c :: rest -> lexString(rest, str + c.ToString())
    | [] -> [LiteralString(str)]
    

let lex (source: string): Token list =
    lexChars (List.ofSeq source)