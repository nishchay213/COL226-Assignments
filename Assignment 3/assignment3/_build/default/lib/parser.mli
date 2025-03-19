type token =
  | IDENTIFIER of (
# 5 "./lib/parser.mly"
        string
# 6 "./lib/parser.mli"
)
  | INT_CONST of (
# 6 "./lib/parser.mly"
        int
# 11 "./lib/parser.mli"
)
  | FLOAT_CONST of (
# 7 "./lib/parser.mly"
        float
# 16 "./lib/parser.mli"
)
  | STRING of (
# 8 "./lib/parser.mly"
        string
# 21 "./lib/parser.mli"
)
  | BOOL_CONST of (
# 9 "./lib/parser.mly"
        bool
# 26 "./lib/parser.mli"
)
  | TYPE of (
# 10 "./lib/parser.mly"
        string
# 31 "./lib/parser.mli"
)
  | MATRIX_VECTOR_MUL
  | TO
  | DO
  | RAISE
  | PRINT_IDENTIFIER of (
# 13 "./lib/parser.mly"
        string
# 40 "./lib/parser.mli"
)
  | INPUT_FILE of (
# 14 "./lib/parser.mly"
        string
# 45 "./lib/parser.mli"
)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MODULO
  | EXPONENT
  | PLUS_FLOAT
  | MINUS_FLOAT
  | TIMES_FLOAT
  | DIV_FLOAT
  | EQUAL
  | NOT_EQUAL
  | LESS
  | GREATER
  | LESS_EQUAL
  | GREATER_EQUAL
  | AND
  | OR
  | NOT
  | VECTOR_ADD
  | VECTOR_DOT
  | VECTOR_CROSS
  | VECTOR_MAG
  | VECTOR_SCAL
  | VECTOR_ANGLE
  | VECTOR_NORM
  | VECTOR_DIM
  | MATRIX_ADD
  | MATRIX_SCAL
  | MATRIX_DET
  | MATRIX_MUL
  | MATRIX_TR
  | MATRIX_ROWS
  | MATRIX_COLS
  | MATRIX_ADJ
  | MATRIX_INV
  | MATRIX_MINOR
  | MATRIX_TRACE
  | NEG
  | TRANS
  | DET
  | MAG
  | ABS
  | SQRT
  | ABS_FLOAT
  | ASSIGN
  | SEMICOLON
  | COMMA
  | IF
  | THEN
  | ELSE
  | FOR
  | WHILE
  | INPUT
  | PRINT
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
