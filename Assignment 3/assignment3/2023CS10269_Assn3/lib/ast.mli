type value =
  | Int of int
  | Float of float
  | Bool of bool
  | VectorInt of int list * int (* Vector values and optional dimension *)
  | MatrixInt of int list list * (int * int) (* Matrix values and optional dimensions (rows, cols) *)
  | VectorFloat of float list * int (* Vector values and optional dimension *)
  | MatrixFloat of float list list * (int * int) (* Matrix values and optional dimensions (rows, cols) *)
  | Identifier of string
(* Binary operators *)
type binary_op =
  (* Integer operations *)
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  (* Float operations *)
  | Exponent
  | AddFloat
  | SubtractFloat
  | MultiplyFloat
  | DivideFloat
  (* Comparison operators *)
  | Equal
  | NotEqual
  | Less
  | Greater
  | LessEqual
  | GreaterEqual
  (* Logical operators *)
  | And
  | Or
  (* Vector operations *)
  | VectorAdd
  | VectorDot
  | VectorCross
  | VectorAngle
  | VectorScale
  (* Matrix operations *)
  | MatrixAdd
  | MatrixMultiply
  | MatrixScale
  | MatrixVectorMultiply

(* Unary operators *)
type unary_op =
  | Negate
  | Not
  | Abs
  | AbsFloat
  | Sqrt
  | VectorMagnitude
  | VectorDimension
  | VectorNormalize
  | MatrixRows
  | MatrixCols
  | MatrixDeterminant
  | MatrixTranspose
  | MatrixAdjoint
  | MatrixInverse
  | MatrixTrace

(* Expressions *)
type expr =
| VectorElement of string * expr
  | MatrixElement of string * expr * expr
  | MatrixMinor of string * expr * expr
  | Value of value
  | BinaryOp of expr * binary_op * expr
  | UnaryOp of unary_op * expr
  | Parenthesis of expr                  (* Expression in parentheses *)

(* Statements *)
type statement =
| Reassign of string * expr
| Assign of string * string * expr
| Reassign_vector of string * expr * expr
  | Reassign_matrix of string * expr * expr * expr
  | Expression of expr
  | If of expr * statement * statement  (* Condition, then branch, optional else branch *)
  | While of expr * statement
  | For of string * expr * expr * statement      (* Initialization, condition, increment, body *)
  | Block of statement list
  | Input of string * string
  | Input_Vec of string * int * string
  | Input_Mat of string * int * int * string
  | InputFile of string * string * string
  | InputFile_Vec of string * int * string * string
  | InputFile_Mat of string * int * int * string * string
  | Print
  | PrintIdentifier of string
  | Raise of string

(* Program is a list of statements *)
type program = statement list

val string_of_value : value -> string
val string_of_binary_op : binary_op -> string
val string_of_unary_op : unary_op -> string
val string_of_expr : expr -> string
(* val string_of_program : program -> string *)
val string_of_statement : statement -> string
