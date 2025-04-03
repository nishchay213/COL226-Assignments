(* Abstract Syntax Tree for the compiler *)


(* Basic types *)
type value =
  | Int of int
  | Float of float
  | Bool of bool
  | VectorInt of int list * int (* Vector values and optional dimension *)
  | MatrixInt of int list list * (int * int)
    (* Matrix values and optional dimensions (rows, cols) *)
  | VectorFloat of float list * int (* Vector values and optional dimension *)
  | MatrixFloat of float list list * (int * int)
    (* Matrix values and optional dimensions (rows, cols) *)
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
  | Parenthesis of expr (* Expression in parentheses *)

(* Statements *)
type statement =
  | Reassign of string * expr
  | Assign of string * string * expr
  | Reassign_vector of string * expr * expr
  | Reassign_matrix of string * expr * expr * expr
  | Expression of expr
  | If of
      expr
      * statement
      * statement (* Condition, then branch, optional else branch *)
  | While of expr * statement
  | For of
      string
      * expr
      * expr
      * statement (* Initialization, condition, increment, body *)
  | Block of statement list
  | Input of string * string
  | Input_Vec of string * int * string
  | Input_Mat of string * int * int * string
  | InputFile of string * string * string
  | InputFile_Vec of string * int * string * string
  | InputFile_Mat of string * int * int * string * string
  | Print
  | PrintIdentifier of string
  | Raise of string (* Raise an exception *)

(* Program is a list of statements *)
type program = statement list

(* Helper functions *)

(* String conversion for debugging *)
let string_of_value = function
  | Int i -> "Int(" ^ string_of_int i ^ ")"
  | Float f -> "Float(" ^ string_of_float f ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | VectorInt (v, dim) ->
      "VectorInt(" ^ string_of_int dim ^ ", ["
      ^ String.concat "; " (List.map string_of_int v)
      ^ "])"
  | MatrixInt (m, (rows, cols)) ->
      "MatrixInt(" ^ string_of_int rows ^ ", " ^ string_of_int cols ^ ", ["
      ^ String.concat "; "
          (List.map
             (fun row ->
               "[" ^ String.concat "; " (List.map string_of_int row) ^ "]")
             m)
      ^ "])"
  | VectorFloat (v, dim) ->
      "VectorFloat(" ^ string_of_int dim ^ ", ["
      ^ String.concat "; " (List.map string_of_float v)
      ^ "])"
  | MatrixFloat (m, (rows, cols)) ->
      "MatrixFloat(" ^ string_of_int rows ^ ", " ^ string_of_int cols ^ ", ["
      ^ String.concat "; "
          (List.map
             (fun row ->
               "[" ^ String.concat "; " (List.map string_of_float row) ^ "]")
             m)
      ^ "])"
  | Identifier id -> "Identifier(" ^ id ^ ")"

let string_of_binary_op = function
  | Add -> "Add"
  | Subtract -> "Subtract"
  | Multiply -> "Multiply"
  | Divide -> "Divide"
  | Modulo -> "Modulo"
  | Exponent -> "EXPONENT"
  | AddFloat -> "AddFloat"
  | SubtractFloat -> "SubtractFloat"
  | MultiplyFloat -> "MultiplyFloat"
  | DivideFloat -> "DivideFloat"
  | Equal -> "Equal"
  | NotEqual -> "NotEqual"
  | Less -> "Less"
  | Greater -> "Greater"
  | LessEqual -> "LessEqual"
  | GreaterEqual -> "GreaterEqual"
  | And -> "And"
  | Or -> "Or"
  | VectorAdd -> "VectorAdd"
  | VectorDot -> "VectorDot"
  | VectorCross -> "VectorCross"
  | VectorAngle -> "VectorAngle"
  | VectorScale -> "VectorScale"
  | MatrixAdd -> "MatrixAdd"
  | MatrixMultiply -> "MatrixMultiply"
  | MatrixScale -> "MatrixScale"
  | MatrixVectorMultiply -> "MatruxVectorMultiply"

let string_of_unary_op = function
  | Negate -> "Negate"
  | Not -> "Not"
  | Abs -> "Abs"
  | AbsFloat -> "AbsFloat"
  | Sqrt -> "Sqrt"
  | VectorMagnitude -> "VectorMagnitude"
  | MatrixRows -> "MatrixRows"
  | MatrixCols -> "MatrixCols"
  | VectorDimension -> "VectorDimension"
  | VectorNormalize -> "VectorNormalize"
  | MatrixDeterminant -> "MatrixDeterminant"
  | MatrixTranspose -> "MatrixTranspose"
  | MatrixAdjoint -> "MatrixAdjoint"
  | MatrixInverse -> "MatrixInverse"
  | MatrixTrace -> "MatrixTrace"

let rec string_of_expr = function
  | Value v -> "Value(" ^ string_of_value v ^ ")"
  | VectorElement (v, e) -> "VectorElement(" ^ v ^ ", " ^ string_of_expr e ^ ")"
  | MatrixElement (v, e1, e2) -> "MatrixElement(" ^  v ^ ", " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | MatrixMinor (v, e1, e2) -> "MatrixMinor(" ^  v ^ ", " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | BinaryOp (e1, op, e2) ->
      "BinaryOp(" ^ string_of_expr e1 ^ ", " ^ string_of_binary_op op ^ ", "
      ^ string_of_expr e2 ^ ")"
  | UnaryOp (op, e) ->
      "UnaryOp(" ^ string_of_unary_op op ^ ", " ^ string_of_expr e ^ ")"
  | Parenthesis e -> "Parenthesis(" ^ string_of_expr e ^ ")"

let rec string_of_statement = function
  | Reassign (id, e) -> "Reassign(" ^ id ^ ", " ^ string_of_expr e ^ ")"
  | Assign (typ, id, e) -> "Assign(" ^ typ ^ ", " ^ id ^ ", " ^ string_of_expr e ^ ")"
  | Reassign_vector (id, e1, e2) ->
      "Reassign_Vector(" ^ id ^ ", " ^ string_of_expr e1 ^ ", "
      ^ string_of_expr e2 ^ ")"
  | Reassign_matrix (id, e1, e2, e3) ->
      "Reassign_Matrix(" ^ id ^ ", " ^ string_of_expr e1 ^ ", "
      ^ string_of_expr e2 ^ ", " ^ string_of_expr e3 ^ ")"
  | Expression e -> "Expression(" ^ string_of_expr e ^ ")"
  | If (cond, then_stmt, else_stmt) ->
      "If(" ^ string_of_expr cond ^ ", "
      ^ string_of_statement then_stmt
      ^ ", "
      ^ string_of_statement else_stmt
      ^ ")"
  | While (cond, body) ->
      "While(" ^ string_of_expr cond ^ ", " ^ string_of_statement body ^ ")"
  | For (init, cond, incr, body) ->
      "For(" ^ init ^ ", " ^ string_of_expr cond ^ ", "
      ^ string_of_expr incr ^ ", " ^ string_of_statement body ^ ")"
  | Block stmts ->
      "Block([" ^ String.concat "; " (List.map string_of_statement stmts) ^ "])"
  | Input (types, id) -> "Input(" ^ types ^ ", " ^ id ^ ")"
  | Input_Vec (types, dim, id) -> "Input_Vec(" ^ types ^ ", " ^ string_of_int dim ^ ", " ^ id ^ ")"
  | Input_Mat (types, rows, cols, id) ->
      "Input_Mat(" ^ types ^ ", " ^ string_of_int rows ^ ", " ^ string_of_int cols ^ ", " ^ id ^ ")"
  | InputFile (types, id, filename) ->
      "InputFile(" ^ types ^ ", " ^ id ^ ", " ^ filename ^ ")"
  | InputFile_Vec (types, dim, id, filename) ->
      "InputFile_Vec(" ^ types ^ ", " ^ string_of_int dim ^ ", " ^ id ^ ", " ^ filename ^ ")"
  | InputFile_Mat (types, rows, cols, id, filename) ->
      "InputFile_Mat(" ^ types ^ ", " ^ string_of_int rows ^ ", " ^ string_of_int cols ^ ", " ^ id ^ ", " ^ filename ^ ")"
  | Print -> "Print"
  | PrintIdentifier id -> "PrintIdentifier(" ^ id ^ ")"
  | Raise msg -> "Raise(" ^ msg ^ ")"

  (* let string_of_program prog =
    try
      (* Perform type checking on the program *)
      let _ = type_program prog in
      (* If type checking passes, convert the program to a string *)
      "Program([" ^ String.concat ";\n" (List.map string_of_statement prog) ^ "])"
    with
    | TypeError ->
        "TypeError" *)