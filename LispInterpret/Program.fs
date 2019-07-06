module Program

open Lexer
open Parser
open Interpreter

let rec repl env =
    try
        printf ">> "
        let (output, updatedEnv) = System.Console.ReadLine() |> lex |> parse |> evalExpressions env
        printExpression (output, updatedEnv) |> System.Console.Out.WriteLine
        repl updatedEnv
    with ex ->
        printf "Error: %s" ex.Message
        repl env

let [<EntryPoint>] main _ =
    let env = createGlobalEnv ()
    System.Console.WriteLine("Welcome to LispInterpret")
    repl env
    0