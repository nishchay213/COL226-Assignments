open Ast
open Interpreter
open Type_checker

let parse_program filename =
  let input = open_in filename in
  let lexbuf = Lexing.from_channel input in
  try
    let program = Parser.program Lexer.token lexbuf in
    close_in input;
    program
  with
  | Parser.Error ->
      close_in input;
      failwith "Syntax error"
  | e ->
      close_in input;
      raise e

let main () =
  if Array.length Sys.argv <> 2 then
    Printf.printf "Usage: %s <source_file>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    try
      let program = parse_program filename in
      (* Type-check the program *)
      type_program program;
      (* Interpret the program *)
      interpret_program program;
      Printf.printf "Program executed successfully.\n"
    with
    | TypeError msg -> Printf.printf "Type error: %s\n" msg
    | RuntimeError msg -> Printf.printf "Runtime error: %s\n" msg
    | Failure msg -> Printf.printf "Error: %s\n" msg
    | e -> Printf.printf "Unexpected error: %s\n" (Printexc.to_string e)

let () = main ()
