(* parser.mli - Token Type Definitions for Lexer *)

(* Define the token type *)
type token =
  (* Input/Output Commands *)
  | INPUT
  | INPUT_FILE of string
  | PRINT
  | PRINT_IDENTIFIER of string
  (* Data Types *)
  | INT_CONST of int
  | FLOAT_CONST of float
  | BOOL_CONST of bool
  | VECTOR_CONST of string
  | MATRIX_CONST of string
  | VECTOR_CONST_WITH_DIM of int * string
  | MATRIX_CONST_WITH_DIM of int * int * string
  (* Identifiers *)
  | IDENTIFIER of string
  (* Arithmetic Operators *)
  | PLUS (* + *)
  | MINUS (* - *)
  | TIMES (* * *)
  | DIV (* / *)
  | ABS (* abs *)
  | MODULO (* % *)
  | VECTOR_ADD
  | VECTOR_DOT
  | VECTOR_SCAL
  | VECTOR_CROSS
  | VECTOR_NORM
  | VECTOR_ANGLE
  | VECTOR_MAG
  | VECTOR_DIM
  | MATRIX_ADD
  | MATRIX_DET
  | MATRIX_TR
  | MATRIX_SCAL
  | MATRIX_MUL
  (* Float Arithmetic Operators *)
  | PLUS_FLOAT (* +. *)
  | MINUS_FLOAT (* -. *)
  | TIMES_FLOAT (* *. *)
  | DIV_FLOAT (* /. *)
  | ABS_FLOAT (* absf *)
  (* Comparison Operators *)
  | EQUAL (* = *)
  | NOT_EQUAL (* != *)
  | LESS (* < *)
  | GREATER (* > *)
  | LESS_EQUAL (* <= *)
  | GREATER_EQUAL (* >= *)
  (* Logical Operators *)
  | NOT (* ! *)
  | AND (* && *)
  | OR (* || *)
  (* Assignment and Control Structures *)
  | ASSIGN (* := *)
  | IF
  | THEN
  | ELSE
  | FOR
  | WHILE
  (* Special Characters *)
  | LBRACE
  | RBRACE (* { } *)
  | SEMICOLON
  | LPAREN
  | RPAREN (* ; *)
  (* End of File *)
  | EOF

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Ast.expr