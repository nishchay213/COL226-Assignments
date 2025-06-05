open Ast
exception TypeError of string

type typ = 
  | TInt
  | TFloat
  | TBool
  | TString
  | TVector of typ * int
  | TMatrix of typ * int * int
  | TError

type env = {
  parent: env option;
  bindings: (string, typ) Hashtbl.t;
}

let new_scope parent = {
  parent = Some parent;
  bindings = Hashtbl.create 8;
}

let global_scope () = {
  parent = None;
  bindings = Hashtbl.create 8;
}

let rec string_of_typ t =
  match t with
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
  | TString -> "string"
  | TVector (typ, dim) -> "vector of " ^ (string_of_typ typ) ^ " with dimension " ^ string_of_int dim
  | TMatrix (typ, rows, cols) -> "matrix of " ^ (string_of_typ typ) ^ " with dimensions " ^ string_of_int rows ^ "x" ^ string_of_int cols
  | TError -> "error"

let rec lookup env id =
  try Hashtbl.find env.bindings id
  with Not_found -> 
    match env.parent with
    | Some parent -> lookup parent id
    | None -> raise (TypeError ("Variable " ^ id ^ " not found in the current scope"))

let assign id typ env =
  if Hashtbl.mem env.bindings id then
    raise (TypeError ("Variable " ^ id ^ " is already defined in the current scope"))
  else
    (Hashtbl.replace env.bindings id typ; env)

let rec type_of e env =
  match e with
  | Value (Int _) -> TInt
  | Value (Float _) ->  TFloat
  | Value (Bool _) -> TBool
  | Value (VectorInt (_, dim)) -> TVector (TInt, dim)
  | Value (MatrixInt (_, (rows, cols))) -> TMatrix (TInt, rows, cols)
  | Value (VectorFloat (_, dim)) -> TVector (TFloat, dim)
  | Value (MatrixFloat (_, (rows, cols))) -> TMatrix (TFloat, rows, cols)
  | Value (Identifier id) -> lookup env id
  | BinaryOp (e1, Add, e2) -> 
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TInt
    else if t1 = TFloat && t2 = TFloat then TFloat
    else raise (TypeError ("Addition type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
  | BinaryOp (e1, Subtract, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TInt
    else if t1 = TFloat && t2 = TFloat then TFloat
    else raise (TypeError ("Subtraction type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
  | BinaryOp (e1, Multiply, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TInt
    else if t1 = TFloat && t2 = TFloat then TFloat
    else raise (TypeError ("Multiplication type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
  | BinaryOp (e1, Divide, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TInt
    else if t1 = TFloat && t2 = TFloat then TFloat
    else raise (TypeError ("Division type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
  | BinaryOp (e1, Modulo, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TInt
    else raise (TypeError ("Modulo operation requires integer types, but got: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
  | BinaryOp (e1, Equal, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = t2 then TBool
    else raise (TypeError ("Equality comparison type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
  | BinaryOp (e1, NotEqual, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = t2 then TBool
    else raise (TypeError ("Inequality comparison type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
  | BinaryOp (e1, Less, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TBool
    else if t1 = TFloat && t2 = TFloat then TBool
    else raise (TypeError ("Less-than comparison type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
  | BinaryOp (e1, Greater, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TBool
    else if t1 = TFloat && t2 = TFloat then TBool
    else raise (TypeError ("Greater-than comparison type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
  | BinaryOp (e1, LessEqual, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TBool
    else if t1 = TFloat && t2 = TFloat then TBool
    else raise (TypeError ("Less-than-or-equal comparison type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
  | BinaryOp (e1, GreaterEqual, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TBool
    else if t1 = TFloat && t2 = TFloat then TBool
    else raise (TypeError ("Greater-than-or-equal comparison type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
  | BinaryOp (e1, And, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TBool && t2 = TBool then TBool
    else raise (TypeError ("Logical AND operation requires boolean types, but got: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
  | BinaryOp (e1, Or, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TBool && t2 = TBool then TBool
    else raise (TypeError ("Logical OR operation requires boolean types, but got: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
  | BinaryOp (e1, Exponent, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TFloat
    else if t1 = TFloat && t2 = TInt then TFloat
    else raise (TypeError ("Exponentiation type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
  | BinaryOp (e1, AddFloat, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TFloat && t2 = TFloat then TFloat
    else raise (TypeError ("Addition of floats type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
  | BinaryOp (e1, SubtractFloat, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TFloat && t2 = TFloat then TFloat
    else raise (TypeError ("Subtraction of floats type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
  | BinaryOp (e1, MultiplyFloat, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TFloat && t2 = TFloat then TFloat
    else raise (TypeError ("Multiplication of floats type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
  | BinaryOp (e1, DivideFloat, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TFloat && t2 = TFloat then TFloat
    else raise (TypeError ("Division of floats type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
  | BinaryOp (e1, VectorAdd, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match t1, t2 with
    | TVector (TInt, dim1), TVector (TInt, dim2) -> if dim1 = dim2 then TVector (TInt, dim1)
    else raise (TypeError ("Vector addition dimension mismatch: " ^ string_of_int dim1 ^ " and " ^ string_of_int dim2))
    | TVector (TFloat, dim1), TVector (TFloat, dim2) -> if dim1 = dim2 then TVector (TFloat, dim1)
    else raise (TypeError ("Vector addition dimension mismatch: " ^ string_of_int dim1 ^ " and " ^ string_of_int dim2))
    | _, _ -> raise (TypeError ("Vector addition type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2)))
  | BinaryOp (e1, VectorDot, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match t1, t2 with
    | TVector (TInt, dim1), TVector (TInt, dim2) -> if dim1 = dim2 then TInt
    else raise (TypeError ("Vector dot product dimension mismatch: " ^ string_of_int dim1 ^ " and " ^ string_of_int dim2))
    | TVector (TFloat, dim1), TVector (TFloat, dim2) -> if dim1 = dim2 then TFloat
    else raise (TypeError ("Vector dot product dimension mismatch: " ^ string_of_int dim1 ^ " and " ^ string_of_int dim2))
    | _, _ -> raise (TypeError ("Vector dot product type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2)))
  | BinaryOp (e1, VectorCross, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match t1, t2 with
    | TVector (TInt, 3), TVector (TInt, 3) -> TVector (TInt, 3)
    | TVector (TFloat, 3), TVector (TFloat, 3) -> TVector (TFloat, 3)
    | _, _ -> raise (TypeError ("Vector cross product type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2)))
  | BinaryOp (e1, VectorAngle, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match t1, t2 with
    | TVector (TInt, dim1), TVector (TInt, dim2) -> if dim1 = dim2 then TFloat
    else raise (TypeError ("Vector angle dimension mismatch: " ^ string_of_int dim1 ^ " and " ^ string_of_int dim2))
    | TVector (TFloat, dim1), TVector (TFloat, dim2) -> if dim1 = dim2 then TFloat
    else raise (TypeError ("Vector angle dimension mismatch: " ^ string_of_int dim1 ^ " and " ^ string_of_int dim2))
    | _, _ -> raise (TypeError ("Vector angle type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2)))
  | BinaryOp (e1, VectorScale, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match t1, t2 with
    | TInt, TVector (TInt, dim) -> TVector (TInt, dim)
    | TFloat, TVector (TFloat, dim) -> TVector (TFloat, dim)
    | _, _ -> raise (TypeError ("Vector scaling type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2)))
  | BinaryOp (e1, MatrixAdd, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match t1, t2 with
    | TMatrix (TInt, rows1, cols1), TMatrix (TInt, rows2, cols2) -> if rows1 = rows2 && cols1 = cols2 then TMatrix (TInt, rows1, cols1)
    else raise (TypeError ("Matrix addition dimension mismatch: " ^ string_of_int rows1 ^ "x" ^ string_of_int cols1 ^ " and " ^ string_of_int rows2 ^ "x" ^ string_of_int cols2))
    | TMatrix (TFloat, rows1, cols1), TMatrix (TFloat, rows2, cols2) -> if rows1 = rows2 && cols1 = cols2 then TMatrix (TFloat, rows1, cols1)
    else raise (TypeError ("Matrix addition dimension mismatch: " ^ string_of_int rows1 ^ "x" ^ string_of_int cols1 ^ " and " ^ string_of_int rows2 ^ "x" ^ string_of_int cols2))
    | _, _ -> raise (TypeError ("Matrix addition type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2)))
  | BinaryOp (e1, MatrixMultiply, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match t1, t2 with
    | TMatrix (TInt, rows1, cols1), TMatrix (TInt, rows2, cols2) -> if cols1 = rows2 then TMatrix (TInt, rows1, cols2)
    else raise (TypeError ("Matrix multiplication dimension mismatch: " ^ string_of_int cols1 ^ " and " ^ string_of_int rows2))
    | TMatrix (TFloat, rows1, cols1), TMatrix (TFloat, rows2, cols2) -> if cols1 = rows2 then TMatrix (TFloat, rows1, cols2)
    else raise (TypeError ("Matrix multiplication dimension mismatch: " ^ string_of_int cols1 ^ " and " ^ string_of_int rows2))
    | _, _ -> raise (TypeError ("Matrix multiplication type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2)))
  | BinaryOp (e1, MatrixScale, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match t1, t2 with
    | TInt, TMatrix (TInt, rows, cols) -> TMatrix (TInt, rows, cols)
    | TFloat, TMatrix (TFloat, rows, cols) -> TMatrix (TFloat, rows, cols)
    | _, _ -> raise (TypeError ("Matrix scaling type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2)))
  | BinaryOp (e1, MatrixVectorMultiply, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match t1, t2 with
    | TMatrix (TInt, rows, cols), TVector (TInt, dim) -> if cols = dim then TVector (TInt, rows)
    else raise (TypeError ("Matrix-vector multiplication dimension mismatch: " ^ string_of_int cols ^ " and " ^ string_of_int dim))
    | TMatrix (TFloat, rows, cols), TVector (TFloat, dim) -> if cols = dim then TVector (TFloat, rows)
    else raise (TypeError ("Matrix-vector multiplication dimension mismatch: " ^ string_of_int cols ^ " and " ^ string_of_int dim))
    | TVector (TInt, dim), TMatrix (TInt, rows, cols) -> if dim = rows then TVector (TInt, cols)
    else raise (TypeError ("Vector-matrix multiplication dimension mismatch: " ^ string_of_int dim ^ " and " ^ string_of_int rows))
    | TVector (TFloat, dim), TMatrix (TFloat, rows, cols) -> if dim = rows then TVector (TFloat, cols)
    else raise (TypeError ("Vector-matrix multiplication dimension mismatch: " ^ string_of_int dim ^ " and " ^ string_of_int rows))
    | _, _ -> raise (TypeError ("Matrix-vector multiplication type mismatch: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2)))
  | UnaryOp (Not, e) ->
    let t = type_of e env in
    if t = TBool then TBool
    else raise (TypeError ("Logical NOT operation requires boolean type, but got: " ^ string_of_typ t))
  | UnaryOp (Negate, e) ->
    let t = type_of e env in
    if t = TInt then TInt
    else if t = TFloat then TFloat
    else raise (TypeError ("Negation operation requires integer or float type, but got: " ^ string_of_typ t))
  | UnaryOp (Abs, e) ->
    let t = type_of e env in
    if t = TInt then TInt
    else if t = TFloat then TFloat
    else raise (TypeError ("Absolute value operation requires integer or float type, but got: " ^ string_of_typ t))
  | UnaryOp (AbsFloat, e) ->
    let t = type_of e env in
    if t = TFloat then TFloat
    else raise (TypeError ("Absolute value operation for floats requires float type, but got: " ^ string_of_typ t))
  | UnaryOp (Sqrt, e) ->
    let t = type_of e env in
    if t = TInt then TFloat
    else if t = TFloat then TFloat
    else raise (TypeError ("Square root operation requires integer or float type, but got: " ^ string_of_typ t))
  | UnaryOp (VectorMagnitude, e) ->
    let t = type_of e env in
    (match t with
    | TVector (TInt, _) -> TFloat
    | TVector (TFloat, _) -> TFloat
    | _ -> raise (TypeError ("Vector magnitude operation requires vector type, but got: " ^ string_of_typ t)))
  | UnaryOp (VectorDimension, e) ->
    let t = type_of e env in
    (match t with
    | TVector (_, _dim) -> TInt
    | _ -> raise (TypeError ("Vector dimension operation requires vector type, but got: " ^ string_of_typ t)))
  | UnaryOp (VectorNormalize, e) ->
    let t = type_of e env in
    (match t with
    | TVector (TInt, dim) -> TVector (TFloat, dim)
    | TVector (TFloat, dim) -> TVector (TFloat, dim)
    | _ -> raise (TypeError ("Vector normalization operation requires vector type, but got: " ^ string_of_typ t)))
  | UnaryOp (MatrixRows, e) ->
    let t = type_of e env in
    (match t with
    | TMatrix (_, _rows, _) -> TInt
    | _ -> raise (TypeError ("Matrix rows operation requires matrix type, but got: " ^ string_of_typ t)))
  | UnaryOp (MatrixCols, e) ->
    let t = type_of e env in
    (match t with
    | TMatrix (_, _, _cols) -> TInt
    | _ -> raise (TypeError ("Matrix columns operation requires matrix type, but got: " ^ string_of_typ t)))
  | UnaryOp (MatrixDeterminant, e) ->
    let t = type_of e env in
    (match t with
    | TMatrix (TInt, rows, cols) -> if rows = cols then TInt
    else raise (TypeError ("Matrix determinant operation requires square matrix, but got: " ^ string_of_int rows ^ "x" ^ string_of_int cols))
    | TMatrix (TFloat, rows, cols) -> if rows = cols then TFloat
    else raise (TypeError ("Matrix determinant operation requires square matrix, but got: " ^ string_of_int rows ^ "x" ^ string_of_int cols))
    | _ -> raise (TypeError ("Matrix determinant operation requires matrix type, but got: " ^ string_of_typ t)))
  | UnaryOp (MatrixTranspose, e) ->
    let t = type_of e env in
    (match t with
    | TMatrix (TInt, rows, cols) -> TMatrix (TInt, cols, rows)
    | TMatrix (TFloat, rows, cols) -> TMatrix (TFloat, cols, rows)
    | _ -> raise (TypeError ("Matrix transpose operation requires matrix type, but got: " ^ string_of_typ t)))
  | UnaryOp (MatrixAdjoint, e) ->
    let t = type_of e env in
    (match t with
    | TMatrix (TInt, rows, cols) -> TMatrix (TInt, rows, cols)
    | TMatrix (TFloat, rows, cols) -> TMatrix (TFloat, rows, cols)
    | _ -> raise (TypeError ("Matrix adjoint operation requires matrix type, but got: " ^ string_of_typ t)))
  | UnaryOp (MatrixInverse, e) ->
    let t = type_of e env in
    (match t with
    | TMatrix (TInt, rows, cols) -> TMatrix (TInt, rows, cols)
    | TMatrix (TFloat, rows, cols) -> TMatrix (TFloat, rows, cols)
    | _ -> raise (TypeError ("Matrix inverse operation requires matrix type, but got: " ^ string_of_typ t)))
  | UnaryOp (MatrixTrace, e) ->
    let t = type_of e env in
    (match t with
    | TMatrix (TInt, rows, cols) -> if rows = cols then TInt
    else raise (TypeError ("Matrix trace operation requires square matrix, but got: " ^ string_of_int rows ^ "x" ^ string_of_int cols))
    | TMatrix (TFloat, rows, cols) -> if rows = cols then TFloat
    else raise (TypeError ("Matrix trace operation requires square matrix, but got: " ^ string_of_int rows ^ "x" ^ string_of_int cols))
    | _ -> raise (TypeError ("Matrix trace operation requires matrix type, but got: " ^ string_of_typ t)))
  | VectorElement (v, e) ->
    let t = type_of e env in
    let vec = lookup env v in
    (match vec with
    | TVector (TInt, _dim) -> if t = TInt then TInt
    else raise (TypeError ("Vector element access requires integer index, but got: " ^ string_of_typ t))
    | TVector (TFloat, _dim) -> if t = TInt then TFloat
    else raise (TypeError ("Vector element access requires integer index, but got: " ^ string_of_typ t))
    | _ -> raise (TypeError ("Vector element access requires vector type, but got: " ^ string_of_typ vec)))
  | MatrixElement (m, e1, e2) ->
    let mat = lookup env m in
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match mat with
    | TMatrix (TInt, _rows, _cols) -> if t1 = TInt && t2 = TInt then TInt
    else raise (TypeError ("Matrix element access requires integer indices, but got: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
    | TMatrix(TFloat, _rows, _cols) -> if t1 = TInt && t2 = TInt then TFloat
    else raise (TypeError ("Matrix element access requires integer indices, but got: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
    | _ -> raise (TypeError ("Matrix element access requires matrix type, but got: " ^ string_of_typ mat)))
  | Parenthesis e -> type_of e env
  | MatrixMinor (m, e1, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    let mat = lookup env m in
    (match mat with
    | TMatrix(TInt, rows, cols) -> if t1 = TInt && t2 = TInt then TMatrix(TInt, rows - 1, cols - 1)
    else raise (TypeError ("Matrix minor operation requires integer indices, but got: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
    | TMatrix(TFloat, rows, cols) -> if t1 = TInt && t2 = TInt then TMatrix(TFloat, rows - 1, cols - 1)
    else raise (TypeError ("Matrix minor operation requires integer indices, but got: " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
    | _ -> raise (TypeError ("Matrix minor operation requires matrix type, but got: " ^ string_of_typ mat)))

let rec type_stmt stmt env = match stmt with
| Reassign (id, e) ->
  let typ = lookup env id in
  let t = type_of e env in
  if typ = t then env
  else raise (TypeError ("Reassignment type mismatch for variable " ^ id ^ ": expected " ^ string_of_typ typ ^ " but got " ^ string_of_typ t))
| Assign (typ_str, id, e) ->
  let t = type_of e env in
  let expected_typ = match typ_str with
    | "int" -> TInt
    | "float" -> TFloat
    | "bool" -> TBool
    | "vec_int" -> (match t with TVector (TInt, _) -> t | _ -> raise (TypeError ("Expected vector of integers for " ^ id)))
    | "vec_float" -> (match t with TVector (TFloat, _) -> t | _ -> raise (TypeError ("Expected vector of floats for " ^ id)))
    | "mat_int" -> (match t with TMatrix (TInt, _, _) -> t | _ -> raise (TypeError ("Expected matrix of integers for " ^ id)))
    | "mat_float" -> (match t with TMatrix (TFloat, _, _) -> t | _ -> raise (TypeError ("Expected matrix of floats for " ^ id)))
    | _ -> raise (TypeError ("Unknown type " ^ typ_str ^ " for variable " ^ id))
  in
  if t = expected_typ then (Hashtbl.replace env.bindings id t; env)
  else raise (TypeError ("Assignment type mismatch for variable " ^ id ^ ": expected " ^ string_of_typ expected_typ ^ " but got " ^ string_of_typ t))
| Reassign_vector (id, e1, e2) ->
  (let typ = lookup env id in
  let t1 = type_of e1 env in
  let t2 = type_of e2 env in
  match typ with 
  | TVector(TInt, _) -> if t1 = TInt && t2 = TInt then env else raise (TypeError ("Reassignment type mismatch for vector " ^ id ^ ": expected int but got " ^ string_of_typ t2))
  | TVector(TFloat, _) -> if t1 = TInt && t2 = TFloat then env else raise (TypeError ("Reassignment type mismatch for vector " ^ id ^ ": expected float but got " ^ string_of_typ t2))
  | _ -> raise (TypeError ("Reassignment type mismatch for vector " ^ id ^ ": expected vector but got " ^ string_of_typ typ)))
| Reassign_matrix (id, e1, e2, e3) ->
  (let typ = lookup env id in
  let t1 = type_of e1 env in
  let t2 = type_of e2 env in
  let t3 = type_of e3 env in
  match typ with
  | TMatrix(TInt, _, _) -> if t1 = TInt && t2 = TInt && t3 = TInt then env else raise (TypeError ("Reassignment type mismatch for matrix " ^ id ^ ": expected int but got " ^ string_of_typ t3))
  | TMatrix(TFloat, _, _) -> if t1 = TInt && t2 = TInt && t3 = TFloat then env else raise (TypeError ("Reassignment type mismatch for matrix " ^ id ^ ": expected float but got " ^ string_of_typ t3))
  | _ -> raise (TypeError ("Reassignment type mismatch for matrix " ^ id ^ ": expected matrix but got " ^ string_of_typ typ)))
| Expression e -> let _ = type_of e env in env
| If (cond, then_stmt, else_stmt) ->
  let t = type_of cond env in
  if t = TBool then
    let _ = type_stmt then_stmt (new_scope env) in
    let _ = type_stmt else_stmt (new_scope env) in
    env
  else raise (TypeError ("If condition type mismatch: expected bool but got " ^ string_of_typ t))
| While (cond, body) ->
  let t = type_of cond env in
  if t = TBool then
    let _ = type_stmt body (new_scope env) in
    env
  else raise (TypeError ("While condition type mismatch: expected bool but got " ^ string_of_typ t))
| For (id, init_expr, cond_expr, body) ->
  let t1 = type_of init_expr env in
  let t2 = type_of cond_expr env in
  if t1 = TInt && t2 = TInt then
    let new_env = new_scope env in
    Hashtbl.replace new_env.bindings id TInt;
    let _ = type_stmt body new_env in
    env
  else raise (TypeError ("For loop type mismatch: expected int but got " ^ string_of_typ t1 ^ " and " ^ string_of_typ t2))
| PrintIdentifier id -> let _ = lookup env id in env
| Block stmts ->
  (let new_env = new_scope env in
  let _ = List.fold_left (fun e stmt -> type_stmt stmt e) new_env stmts in env)
| Input (types, id) -> (match types with
  | "int" -> (Hashtbl.replace env.bindings id TInt; env)
  | "float" -> (Hashtbl.replace env.bindings id TFloat; env)
  | "bool" -> (Hashtbl.replace env.bindings id TBool; env)
  | _ -> raise (TypeError ("Unknown input type " ^ types ^ " for variable " ^ id)))
| Input_Vec (types, dim, id) -> (match types with
  | "vec_int" -> (Hashtbl.replace env.bindings id (TVector (TInt, dim)); env)
  | "vec_float" -> (Hashtbl.replace env.bindings id (TVector (TFloat, dim)); env)
  | _ -> raise (TypeError ("Unknown input type " ^ types ^ " for vector " ^ id)))
| Input_Mat (types, rows, cols, id) -> (match types with
  | "mat_int" -> (Hashtbl.replace env.bindings id (TMatrix (TInt, rows, cols)); env)
  | "mat_float" -> (Hashtbl.replace env.bindings id (TMatrix (TFloat, rows, cols)); env)
  | _ -> raise (TypeError ("Unknown input type " ^ types ^ " for matrix " ^ id)))
| InputFile (types, id, _filename) -> (match types with
  | "int" -> (Hashtbl.replace env.bindings id TInt; env)
  | "float" -> (Hashtbl.replace env.bindings id TFloat; env)
  | "bool" -> (Hashtbl.replace env.bindings id TBool; env)
  | _ -> raise (TypeError ("Unknown input file type " ^ types ^ " for variable " ^ id)))
| InputFile_Vec (types, dim, id, _filename) -> (match types with
  | "vec_int" -> (Hashtbl.replace env.bindings id (TVector (TInt, dim)); env)
  | "vec_float" -> (Hashtbl.replace env.bindings id (TVector (TFloat, dim)); env)
  | _ -> raise (TypeError ("Unknown input file type " ^ types ^ " for vector " ^ id)))
| InputFile_Mat (types, rows, cols, id, _filename) -> (match types with
  | "mat_int" -> (Hashtbl.replace env.bindings id (TMatrix (TInt, rows, cols)); env)
  | "mat_float" -> (Hashtbl.replace env.bindings id (TMatrix (TFloat, rows, cols)); env)
  | _ -> raise (TypeError ("Unknown input file type " ^ types ^ " for matrix " ^ id)))
| _ -> env



let type_program prog =
  let env = global_scope () in
  let _ = List.fold_left (fun e stmt -> type_stmt stmt e) env prog in
  ()
