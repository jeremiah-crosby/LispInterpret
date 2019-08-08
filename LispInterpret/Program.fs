module Program

open Lexer
open Syntax
open Parser
open Interpreter

let readExpression (reader: System.IO.TextReader) =
    let mutable input = ""
    while (not (input.EndsWith(';'))) do
        input <- String.concat "" [input; reader.ReadLine()]
    if (String.length input) = 0 then
        input
    else
        input.Substring(0, input.Length - 1)

let rec repl env =
    try
        printf "\n>> "
        let output = readExpression (System.Console.In) |> lex |> parse |> evalExpressions env
        printExpression (output, env) |> System.Console.Out.WriteLine
        repl env
    with ex ->
        printf "Error: %s\n" ex.Message
        repl env

let [<EntryPoint>] main _ =
    let env = createGlobalEnv ()
    env.Intrinsics := env.Intrinsics.Value.Add("quit", (fun _ _ -> System.Environment.Exit(0); NilExpr))
    System.Console.WriteLine("Welcome to LispInterpret")
    repl env
    0