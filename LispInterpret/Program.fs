module Program

open Lexer
open Syntax
open Parser
open Interpreter

let load filename env =
    let reader = System.IO.File.OpenText(filename)
    let source = reader.ReadToEnd()
    let tokens = lex source
    let parsed = parse tokens
    let result = evalExpressions env parsed
    printExpression (result, env) |> System.Console.Out.WriteLine
    reader.Close()

let readExpression (reader: System.IO.TextReader) =
    let mutable input = ""
    while (not (input.EndsWith(';'))) do
        input <- String.concat "" [input; reader.ReadLine()]
    if (String.length input) = 0 then
        input
    else
        input.Substring(0, input.Length - 1)

let rec repl env reader =
    try
        printf "\n>> "
        let output = readExpression (reader) |> lex |> parse |> evalExpressions env
        printExpression (output, env) |> System.Console.Out.WriteLine
        repl env reader
    with ex ->
        printf "Error: %s\n" ex.Message
        repl env reader

let [<EntryPoint>] main _ =
    let env = createGlobalEnv ()
    env.Intrinsics := env.Intrinsics.Value.Add("quit", (fun _ _ -> System.Environment.Exit(0); NilExpr))
    env.Intrinsics := env.Intrinsics.Value.Add("load", (fun args env ->
                                                            match args with
                                                            | [StringExpr filename] ->
                                                                load filename env
                                                                NilExpr))
    System.Console.WriteLine("Welcome to LispInterpret")
    repl env System.Console.In
    0