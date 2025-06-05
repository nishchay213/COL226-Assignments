open Assignment3

let string_of_program prog =
    try
      (* Perform type checking on the program *)
      let _ = Type_checker.type_program prog in
      (* If type checking passes, convert the program to a string *)
      ""
    with
    | Type_checker.TypeError msg ->
        Printf.printf "Type error: %s\n" msg;
        exit 1
    

let main () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    try
      let in_channel = open_in filename in
      let lexbuf = Lexing.from_channel in_channel in
      let ast = Parser.program Lexer.token lexbuf in
      close_in in_channel;
      Printf.printf "Parsed AST:\n%s\n" (string_of_program ast);
      try
        Interpreter.interpret_program ast;
        Printf.printf "Program executed successfully.\n"
      with
      | Interpreter.RuntimeError msg -> Printf.printf "Runtime error: %s\n" msg
    with
    | Sys_error msg -> Printf.printf "File error: %s\n" msg
    | Lexer.Eof -> print_endline "Error: Unexpected end of input."
    | exn -> Printf.printf "Error: %s\n" (Printexc.to_string exn)

let () = main ()


(* 
the significant changes which I made was 
1. Changed the syntax for declaration to distinguish it from reassignment.
2. Change how scopes are handled in type_checker, in interpreter also, scopes are implemented in similar way.
3. First I had just added token for Input(filename), but not entirely implemented it, which I have done now
4. Added error messages in typechecker.

 *)