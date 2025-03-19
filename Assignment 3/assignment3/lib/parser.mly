%{
open Ast
%}

%token <string> IDENTIFIER
%token <int> INT_CONST
%token <float> FLOAT_CONST
%token <string> STRING
%token <bool> BOOL_CONST
%token <string> TYPE
%token MATRIX_VECTOR_MUL
%token TO DO RAISE
%token <string> PRINT_IDENTIFIER
%token <string> INPUT_FILE

/* Arithmetic Operators */
%token PLUS MINUS TIMES DIV MODULO EXPONENT
%token PLUS_FLOAT MINUS_FLOAT TIMES_FLOAT DIV_FLOAT

/* Comparison Operators */
%token EQUAL NOT_EQUAL LESS GREATER LESS_EQUAL GREATER_EQUAL

/* Logical Operators */
%token AND OR NOT

/* Vector and Matrix Operations */
%token VECTOR_ADD VECTOR_DOT VECTOR_CROSS VECTOR_MAG VECTOR_SCAL VECTOR_ANGLE VECTOR_NORM VECTOR_DIM
%token MATRIX_ADD MATRIX_SCAL MATRIX_DET MATRIX_MUL MATRIX_TR MATRIX_DET MATRIX_ROWS MATRIX_COLS MATRIX_ADJ MATRIX_INV MATRIX_MINOR MATRIX_TRACE

/* Unary Operators */
%token NEG TRANS DET MAG ABS SQRT ABS_FLOAT

/* Control and Assignment */
%token ASSIGN SEMICOLON COMMA
%token IF THEN ELSE FOR WHILE INPUT PRINT
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token EOF

/* Operator Precedence */
%right ASSIGN RAISE
%left OR
%left AND
%left EQUAL NOT_EQUAL
%left LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS
%left TIMES DIV MODULO
%left EXPONENT
%left MATRIX_VECTOR_MUL
%left VECTOR_ADD VECTOR_DOT VECTOR_CROSS VECTOR_SCAL VECTOR_NORM VECTOR_DIM VECTOR_ANGLE
%left MATRIX_ADD MATRIX_SCAL MATRIX_MUL
%right NEG NOT TRANS DET MAG ABS SQRT VECTOR_MAG MATRIX_DET MATRIX_TR MATRIX_ROWS MATRIX_COLS MATRIX_ADJ MATRIX_INV MATRIX_TRACE

%start program
%type <Ast.program> program

%%

program:
  | stmt_list EOF { $1 }

stmt_list:
  | stmt SEMICOLON stmt_list { $1 :: $3 }
  | stmt SEMICOLON           { [$1] }    (* Allow optional semicolon *)

stmt:
  | IDENTIFIER ASSIGN expr { Assign($1, $3) }
  | IDENTIFIER LBRACKET expr RBRACKET ASSIGN expr { Reassign_vector($1, $3, $6) }
  | IDENTIFIER LBRACKET expr RBRACKET LBRACKET expr RBRACKET ASSIGN expr { Reassign_matrix($1, $3, $6, $9) }
  | IF expr THEN stmt ELSE stmt { If($2, $4, $6) }
  | FOR IDENTIFIER ASSIGN expr TO expr DO stmt { For($2, $4, $6, $8) }
  | WHILE expr DO stmt { While($2, $4) }
  | PRINT_IDENTIFIER { PrintIdentifier($1 ) }
  | INPUT_FILE LPAREN STRING RPAREN { InputFile($3) }
  | TYPE IDENTIFIER ASSIGN INPUT{ Input ($1, $2) }
  | TYPE INT_CONST IDENTIFIER ASSIGN INPUT { Input_Vec ($1, $2, $3) }
  | TYPE INT_CONST INT_CONST  IDENTIFIER ASSIGN INPUT { Input_Mat ($1, $2, $3, $4) }
  | RAISE IDENTIFIER { Raise($2) }
  | block { $1 }

block:
  | LBRACE stmt_list RBRACE { Block($2) }

expr:
  | scalar_expr { $1 }


  

scalar_expr:
  | INT_CONST { Value(Int($1)) }
  | FLOAT_CONST { Value(Float($1)) }
  | BOOL_CONST { Value(Bool($1)) }
  | IDENTIFIER { Value(Identifier($1)) }
  | IDENTIFIER LBRACKET scalar_expr RBRACKET { VectorElement ($1, $3)}
  | IDENTIFIER LBRACKET scalar_expr RBRACKET LBRACKET scalar_expr RBRACKET {MatrixElement ($1, $3, $6)}
  | MATRIX_MINOR IDENTIFIER LBRACKET scalar_expr RBRACKET LBRACKET scalar_expr RBRACKET { MatrixMinor($2, $4, $7) }
  | vector_expr { $1 }
  | matrix_expr { $1 }
  | scalar_expr PLUS scalar_expr { BinaryOp($1, Add, $3) }
  | scalar_expr MINUS scalar_expr { BinaryOp($1, Subtract, $3) }
  | scalar_expr TIMES scalar_expr { BinaryOp($1, Multiply, $3) }
  | scalar_expr DIV scalar_expr { BinaryOp($1, Divide, $3) }
  | scalar_expr EXPONENT scalar_expr { BinaryOp($1, Exponent, $3) }
  | scalar_expr MODULO scalar_expr { BinaryOp($1, Modulo, $3) }
  | scalar_expr EQUAL scalar_expr { BinaryOp($1, Equal, $3) }
  | scalar_expr NOT_EQUAL scalar_expr { BinaryOp($1, NotEqual, $3) }
  | scalar_expr LESS scalar_expr { BinaryOp($1, Less, $3) }
  | scalar_expr GREATER scalar_expr { BinaryOp($1, Greater, $3) }
  | scalar_expr LESS_EQUAL scalar_expr { BinaryOp($1, LessEqual, $3) }
  | scalar_expr GREATER_EQUAL scalar_expr { BinaryOp($1, GreaterEqual, $3) }
    | scalar_expr AND scalar_expr { BinaryOp($1, And, $3) }
    | scalar_expr OR scalar_expr { BinaryOp($1, Or, $3) }
    | scalar_expr VECTOR_ADD scalar_expr { BinaryOp($1, VectorAdd, $3) }
    | scalar_expr VECTOR_DOT scalar_expr { BinaryOp($1, VectorDot, $3) }
    | scalar_expr VECTOR_CROSS scalar_expr { BinaryOp($1, VectorCross, $3) }
    | scalar_expr VECTOR_SCAL scalar_expr { BinaryOp($1, VectorScale, $3) }
    | scalar_expr VECTOR_ANGLE scalar_expr { BinaryOp($1, VectorAngle, $3) }
    | scalar_expr MATRIX_VECTOR_MUL scalar_expr { BinaryOp($1, MatrixVectorMultiply, $3) }
    | VECTOR_DIM scalar_expr { UnaryOp(VectorDimension, $2) }
    | VECTOR_NORM scalar_expr { UnaryOp(VectorNormalize, $2) }
    | VECTOR_MAG scalar_expr { UnaryOp(VectorMagnitude, $2) }
  
    | scalar_expr MATRIX_ADD scalar_expr { BinaryOp($1, MatrixAdd, $3) }
    | scalar_expr MATRIX_SCAL scalar_expr { BinaryOp($1, MatrixScale, $3) }
    | scalar_expr MATRIX_MUL scalar_expr { BinaryOp($1, MatrixMultiply, $3) }
    | MATRIX_DET scalar_expr { UnaryOp(MatrixDeterminant, $2) }
    | MATRIX_TR scalar_expr { UnaryOp(MatrixTranspose, $2) }
    | MATRIX_ROWS scalar_expr { UnaryOp(MatrixRows, $2) }
    | MATRIX_COLS scalar_expr { UnaryOp(MatrixCols, $2) }
    | MATRIX_ADJ scalar_expr { UnaryOp(MatrixAdjoint, $2) }
    | MATRIX_INV scalar_expr { UnaryOp(MatrixInverse, $2) }
    | MATRIX_TRACE scalar_expr { UnaryOp(MatrixTrace, $2) }
    
  | NOT scalar_expr { UnaryOp(Not, $2) }
  | NEG scalar_expr { UnaryOp(Negate, $2) }
  | ABS scalar_expr { UnaryOp(Abs, $2) }
  | SQRT scalar_expr { UnaryOp(Sqrt, $2) }
  
  | LPAREN scalar_expr RPAREN { $2 }

vector_expr:
  | INT_CONST LBRACKET int_vector_element_list RBRACKET { Value(VectorInt($3, $1)) }
  | INT_CONST LBRACKET float_vector_element_list RBRACKET { Value(VectorFloat($3, $1)) }
  
matrix_expr:
  | INT_CONST INT_CONST LBRACKET int_matrix_element_list RBRACKET { Value(MatrixInt($4, ($1, $2))) }
  | INT_CONST INT_CONST LBRACKET float_matrix_element_list RBRACKET { Value(MatrixFloat($4, ($1, $2))) }
  

/* Vectors contain only integers or floats */
int_vector_element_list:
  | INT_CONST { [$1] }
  | INT_CONST COMMA int_vector_element_list { $1 :: $3 }
  
float_vector_element_list:
    | FLOAT_CONST { [$1] }
    | FLOAT_CONST COMMA float_vector_element_list { $1 :: $3 }

/* Matrices contain only vectors of integers or floats */
int_matrix_element_list:
  | LBRACKET int_vector_element_list RBRACKET { [$2] }
  | LBRACKET int_vector_element_list RBRACKET COMMA int_matrix_element_list { $2 :: $5 }

float_matrix_element_list:
  | LBRACKET float_vector_element_list RBRACKET { [$2] }
  | LBRACKET float_vector_element_list RBRACKET COMMA float_matrix_element_list { $2 :: $5 }
