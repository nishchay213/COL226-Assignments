{
(* Import token definitions from parser.mli *)
open Parser
exception Eof
}


  (* Many things will be handled later, but still I have given support to whitespace, vector and matrix with or without dimensions. This assignment was supposed to be open ended, but then they started to put restrictions which I think violated the principle of Occam's razor as simple lexer could be used to build powerful grammar. Just one man's opinion. *)


let digit = ['0'-'9']
let digits = digit+
let signed_digits = ['+''-']?digits
let whitespace = [' ' '\t' '\n']
let character = ['a'-'z' 'A'-'Z']
let identifier_char = character | digit | '_'
let vector = "[" whitespace* signed_digits ('.'digits*)? 
    (whitespace* "," whitespace* signed_digits ('.'digits*)?)* 
    whitespace* "]"
let scientific_deno = signed_digits ("." digits)? "e" signed_digits
let file_name = (character|digit)+ "." (character|digit)+
(* Define the lexer *)

(* Multi-line comment rule *)
rule comment = parse
  | "*/" { token lexbuf }   (* End of comment, return to normal token parsing *)
  | _    { comment lexbuf } (* Consume and continue *)

and token = parse
  (* Whitespace & Comments *)
  | whitespace+ { token lexbuf }  (* Ignore whitespace *)
  | "//" [^ '\n']*  { token lexbuf }  (* Ignore single-line comments *)
  | "/*"  { comment lexbuf }          (* Handle multi-line comments *)

  (* Input and Print Commands *)
  | "Input(" whitespace* (file_name as file) whitespace* ")" 
    { INPUT_FILE file }
  | "Input(" whitespace* ")" { INPUT }
  | "Print(" whitespace* ((identifier_char)+ as id_name) whitespace* ")" 
    { PRINT_IDENTIFIER id_name }
  | "Print(" whitespace* ")" { PRINT }

  (* Numbers *)
  | signed_digits as num { INT_CONST (int_of_string num) }
  | signed_digits "." digit* as float_num { FLOAT_CONST (float_of_string float_num) }
  | scientific_deno as scientific_num { FLOAT_CONST (float_of_string scientific_num) }
  (* Boolean values *)
  | "T" { BOOL_CONST true }
  | "F" { BOOL_CONST false }

  (* Arithmetic Operators *)
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "abs" { ABS }

  | "+." { PLUS_FLOAT }
  | "-." { MINUS_FLOAT }
  | "*." { TIMES_FLOAT }
  | "/." { DIV_FLOAT }
  | "absf" { ABS_FLOAT }
  (* Comparison Operators *)
  | "=" { EQUAL }
  | "<" { LESS }
  | ">" { GREATER }
  | "<=" { LESS_EQUAL }
  | ">=" { GREATER_EQUAL }
  | "!=" { NOT_EQUAL }

  (* Logical Operators *)
  | "!" { NOT }
  | "&&" { AND }
  | "||" { OR }

  (* Modulo Operator *)
  | "%" { MODULO }
  

  (* Keywords & Symbols *)
  | "+v" { VECTOR_ADD }
  | ".v" { VECTOR_DOT }
  | "scal_v" { VECTOR_SCAL }
  | "cross_v" { VECTOR_CROSS }
  | "angle_v" { VECTOR_ANGLE }
  | "mag_v" { VECTOR_MAG }
  | "dim_v" { VECTOR_DIM }
  | "norm_v" { VECTOR_NORM }
  | "+m" { MATRIX_ADD }
  | "scal_m" { MATRIX_SCAL }
  | "det_m" { MATRIX_DET }
  | "TR" { MATRIX_TR }  
  | "*m" { MATRIX_MUL}
  | ":="   { ASSIGN } 
  | "if"    { IF }
  | "then"  { THEN }
  | "else"  { ELSE }
  | "for"   { FOR }
  | "while" { WHILE }
  | "("     { LPAREN }
  | ")"     { RPAREN }
  | "{"     { LBRACE }
  | "}"     { RBRACE }
  | "["     { LBRACKET }
  | "]"     { RBRACKET }
  | ","     { COMMA }
  | ";"     { SEMICOLON }

  (* End of file *)
  | eof { EOF }

  (* Identifiers *)
  | (character identifier_char*) as id { IDENTIFIER id }



(* x := 5 + 3 * 2;
y := 4.5 +. 2.5;
z := x *m y; *)

(* T && F || !T;
x := 4 > 3 && 2 < 5; *)

(* v1 := [1, 2, 3];
v2 := [4.5, 5.5, 6.5];
result := v1 +v v2;
dot_prod := v1 .v v2; *)

(* M := 2 2 [[1, 2], [3, 4]];
N := 2 2 [[5, 6], [7, 8]];
P := M +m N; *)

(* Input();
Input(file.txt);
Print(x);
Print();
*)

(* if x > 5 then y := 10 else y := 0;
for (i := 1; i <= 10; i := i + 1) { Print(i); }
*)

(* Input(file1.txt);
x := 10;
y := 2.5;
if x > 5 && y < 3.0 then {
    z := x *m y;
    Print(z);
} else {
    z := [1, 2, 3] +v [4, 5, 6];
    Print(z);
}
 *)

 (* Input(matrix.txt);
A := [1, 2, 3];
B := [[1, 0], [0, 1]];
Print(result);
/* This is a multi-line
   comment */
if A = B then {
  Print();
}
 *)



