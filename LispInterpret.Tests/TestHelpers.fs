module TestHelpers

open Lexer
open Syntax
open Parser
open Interpreter

// Tokenizes and parses a source string, then evaluates it. The result
// of the last expression is returned, along with the environment state.
let evalString source =
    let tokens = lex source
    let parsed = parse tokens
    evalExpressions parsed (createGlobalEnv ())