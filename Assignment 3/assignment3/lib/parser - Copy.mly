%{
open Ast
%}

%token <string> IDENTIFIER
%token <int> INT_CONST
%token <float> FLOAT_CONST
%token <string> STRING
%token <bool> BOOL_CONST


%token TO DO
%token <string> PRINT_IDENTIFIER
%token <string> INPUT_FILE

/* Arithmetic Operators */
%token PLUS MINUS TIMES DIV MODULO
%token PLUS_FLOAT MINUS_FLOAT TIMES_FLOAT DIV_FLOAT

/* Comparison Operators */
%token EQUAL NOT_EQUAL LESS GREATER LESS_EQUAL GREATER_EQUAL

/* Logical Operators */
%token AND OR NOT

/* Vector and Matrix Operations */
%token VECTOR_ADD VECTOR_DOT VECTOR_CROSS VECTOR_MAG VECTOR_SCAL VECTOR_ANGLE VECTOR_NORM VECTOR_DIM
%token MATRIX_ADD MATRIX_SCAL MATRIX_DET MATRIX_MUL MATRIX_TR MATRIX_DET

/* Unary Operators */
%token NEG TRANS DET MAG ABS SQRT ABS_FLOAT

/* Control and Assignment */
%token ASSIGN SEMICOLON COMMA
%token IF THEN ELSE FOR WHILE DO INPUT PRINT
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token EOF

/* Operator Precedence */
%right ASSIGN
%left OR
%left AND
%left EQUAL NOT_EQUAL
%left LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS
%left TIMES DIV MODULO
%left VECTOR_ADD VECTOR_DOT VECTOR_CROSS VECTOR_SCAL VECTOR_NORM VECTOR_DIM
%left MATRIX_ADD MATRIX_SCAL MATRIX_MUL
%right NEG NOT TRANS DET MAG ABS SQRT

%start program
%type <Ast.program> program

%%

program:
  | stmt_list EOF { $1 }

stmt_list:
  | stmt SEMICOLON stmt_list { $1 :: $3 }

stmt:
  | IDENTIFIER ASSIGN expr { Assign($1, $3) }
  | IF expr THEN stmt ELSE stmt { If($2, $4, $6) }
  | FOR expr ASSIGN expr TO expr DO stmt { For($2, $4, $6, $8) }
  | WHILE expr DO stmt { While($2, $4) }
  | PRINT_IDENTIFIER LPAREN IDENTIFIER RPAREN { PrintIdentifier($3) }
  | INPUT_FILE LPAREN STRING RPAREN { InputFile($3) }
  | INPUT LPAREN RPAREN { Input }

expr:
  | scalar_expr { $1 }
  | vector_expr { $1 }
  | matrix_expr { $1 }

scalar_expr:
  | INT_CONST { Value(Int($1)) }
  | FLOAT_CONST { Value(Float($1)) }
  | BOOL_CONST { Value(Bool($1)) }
  | IDENTIFIER { Value(Identifier($1)) }
  
  | scalar_expr PLUS scalar_expr { BinaryOp($1, Add, $3) }
  | scalar_expr MINUS scalar_expr { BinaryOp($1, Subtract, $3) }
  | scalar_expr TIMES scalar_expr { BinaryOp($1, Multiply, $3) }
  | scalar_expr DIV scalar_expr { BinaryOp($1, Divide, $3) }
  | scalar_expr MODULO scalar_expr { BinaryOp($1, Modulo, $3) }
  | scalar_expr EQUAL scalar_expr { BinaryOp($1, Equal, $3) }
  | scalar_expr NOT_EQUAL scalar_expr { BinaryOp($1, NotEqual, $3) }
  | scalar_expr LESS scalar_expr { BinaryOp($1, Less, $3) }
  | scalar_expr GREATER scalar_expr { BinaryOp($1, Greater, $3) }
  | scalar_expr LESS_EQUAL scalar_expr { BinaryOp($1, LessEqual, $3) }
  | scalar_expr GREATER_EQUAL scalar_expr { BinaryOp($1, GreaterEqual, $3) }
    | scalar_expr AND scalar_expr { BinaryOp($1, And, $3) }
    | scalar_expr OR scalar_expr { BinaryOp($1, Or, $3) }
  | NOT scalar_expr { UnaryOp(Not, $2) }
  | NEG scalar_expr { UnaryOp(Negate, $2) }
  | ABS scalar_expr { UnaryOp(Abs, $2) }
  | SQRT scalar_expr { UnaryOp(Sqrt, $2) }
  
  | LPAREN scalar_expr RPAREN { $2 }

vector_expr:
  | INT_CONST LBRACKET int_vector_element_list RBRACKET { Value(VectorInt($3, $1)) }
  | INT_CONST LBRACKET float_vector_element_list RBRACKET { Value(VectorFloat($3, $1)) }
  | LPAREN vector_expr VECTOR_ADD vector_expr RPAREN { BinaryOp($2, VectorAdd, $4) }
  | LPAREN vector_expr VECTOR_DOT vector_expr RPAREN { BinaryOp($2, VectorDot, $4) }
  | LPAREN vector_expr VECTOR_CROSS vector_expr RPAREN { BinaryOp($2, VectorCross, $4) }
  | LPAREN vector_expr VECTOR_SCAL scalar_expr RPAREN { BinaryOp($2, VectorScale, $4) }
  | LPAREN vector_expr VECTOR_ANGLE vector_expr RPAREN { BinaryOp($2, VectorAngle, $4) }
  | VECTOR_DIM vector_expr { UnaryOp(VectorDimension, $2) }
  | VECTOR_NORM LPAREN vector_expr RPAREN { UnaryOp(VectorNormalize, $3) }
  | VECTOR_MAG LPAREN vector_expr RPAREN { UnaryOp(VectorMagnitude, $3) }

matrix_expr:
  | INT_CONST INT_CONST LBRACKET int_matrix_element_list RBRACKET { Value(MatrixInt($4, ($1, $2))) }
  | INT_CONST INT_CONST LBRACKET float_matrix_element_list RBRACKET { Value(MatrixFloat($4, ($1, $2))) }
  | LPAREN matrix_expr MATRIX_ADD matrix_expr RPAREN { BinaryOp($2, MatrixAdd, $4) }
  | LPAREN matrix_expr MATRIX_SCAL scalar_expr RPAREN { BinaryOp($2, MatrixScale, $4) }
  | LPAREN matrix_expr MATRIX_MUL matrix_expr RPAREN { BinaryOp($2, MatrixMultiply, $4) }
  | MATRIX_DET LPAREN matrix_expr RPAREN { UnaryOp(MatrixDeterminant, $3) }
  | MATRIX_TR LPAREN matrix_expr RPAREN { UnaryOp(MatrixTranspose, $3) }

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
