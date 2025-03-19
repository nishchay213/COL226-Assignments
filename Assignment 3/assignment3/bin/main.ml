open Assignment3

let string_of_program prog =
    try
      (* Perform type checking on the program *)
      let _ = Type_checker.type_program prog in
      (* If type checking passes, convert the program to a string *)
      "Program([" ^ String.concat ";\n" (List.map Ast.string_of_statement prog) ^ "])"
    with
    | Type_checker.TypeError ->
        "TypeError"

let main () =
  let lexbuf = Lexing.from_channel stdin in
  try
    let ast = Parser.program Lexer.token lexbuf in
    Printf.printf "Parsed AST:\n%s\n" (string_of_program ast)
  with
  | Lexer.Eof -> print_endline "Error: Unexpected end of input."
  | exn -> Printf.printf "Error: %s\n" (Printexc.to_string exn)

let () = main ()