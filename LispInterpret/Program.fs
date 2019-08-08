module Program

open Lexer
open Parser
open Interpreter

let readExpression () =
    let mutable input = ""
    while (not (input.EndsWith(';'))) do
        input <- String.concat "" [input; System.Console.ReadLine()]
    if (String.length input) = 0 then
        input
    else
        input.Substring(0, input.Length - 1)

let rec repl env =
    try
        printf "\n>> "
        let output = readExpression () |> lex |> parse |> evalExpressions env
        printExpression (output, env) |> System.Console.Out.WriteLine
        repl env
    with ex ->
        printf "Error: %s\n" ex.Message
        repl env

let [<EntryPoint>] main _ =
    let env = createGlobalEnv ()
    System.Console.WriteLine("Welcome to LispInterpret")
    repl env
    0