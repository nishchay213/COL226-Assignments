open Ast 
exception TypeError
type typ = 
  | TInt
  | TFloat
  | TBool
  | TString
  | TVector of typ * int
  | TMatrix of typ * int * int
  | TError

type env = (string * typ) list

let lookup env id = 
 try List.assoc id env
with Not_found -> Printf.printf "notfound\n"; raise TypeError

let rec type_of e env =
  match e with
  Value (Int _) -> TInt
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
    else raise TypeError
  | BinaryOp (e1, Subtract, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TInt
    else if t1 = TFloat && t2 = TFloat then TFloat
    else raise TypeError
  | BinaryOp (e1, Multiply, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TInt
    else if t1 = TFloat && t2 = TFloat then TFloat
    else raise TypeError
  | BinaryOp (e1, Divide, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TInt
    else if t1 = TFloat && t2 = TFloat then TFloat
    else raise TypeError
  | BinaryOp (e1, Modulo, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TInt
    else raise TypeError
  | BinaryOp (e1, Equal, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = t2 then TBool
    else raise TypeError
  | BinaryOp (e1, NotEqual, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = t2 then TBool
    else raise TypeError
  | BinaryOp (e1, Less, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TBool
    else if t1 = TFloat && t2 = TFloat then TBool
    else raise TypeError
  | BinaryOp (e1, Greater, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TBool
    else if t1 = TFloat && t2 = TFloat then TBool
    else raise TypeError
  | BinaryOp (e1, LessEqual, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TBool
    else if t1 = TFloat && t2 = TFloat then TBool
    else raise TypeError
  | BinaryOp (e1, GreaterEqual, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TBool
    else if t1 = TFloat && t2 = TFloat then TBool
    else raise TypeError
  | BinaryOp (e1, And, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TBool && t2 = TBool then TBool
    else raise TypeError
  | BinaryOp (e1, Or, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TBool && t2 = TBool then TBool
    else raise TypeError
  | BinaryOp (e1, Exponent, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TInt && t2 = TInt then TFloat
    else if t1 = TFloat && t2 = TInt then TFloat
    else raise TypeError
  | BinaryOp (e1, AddFloat, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TFloat && t2 = TFloat then TFloat
    else raise TypeError
  | BinaryOp (e1, SubtractFloat, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TFloat && t2 = TFloat then TFloat
    else raise TypeError
  | BinaryOp (e1, MultiplyFloat, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TFloat && t2 = TFloat then TFloat
    else raise TypeError
  | BinaryOp (e1, DivideFloat, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    if t1 = TFloat && t2 = TFloat then TFloat
    else raise TypeError
  | BinaryOp (e1, VectorAdd, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match t1, t2 with
    | TVector (TInt, dim1), TVector (TInt, dim2) -> if dim1 = dim2 then TVector (TInt, dim1)
    else raise TypeError
    | TVector (TFloat, dim1), TVector (TFloat, dim2) -> if dim1 = dim2 then TVector (TFloat, dim1)
    else raise TypeError
    | _, _ -> raise TypeError)
  | BinaryOp (e1, VectorDot, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match t1, t2 with
    | TVector (TInt, dim1), TVector (TInt, dim2) -> if dim1 = dim2 then TInt
    else raise TypeError
    | TVector (TFloat, dim1), TVector (TFloat, dim2) -> if dim1 = dim2 then TFloat
    else raise TypeError
    | _, _ -> raise TypeError)
  | BinaryOp (e1, VectorCross, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match t1, t2 with
    | TVector (TInt, 3), TVector (TInt, 3) -> TVector (TInt, 3)
    | TVector (TFloat, 3), TVector (TFloat, 3) -> TVector (TFloat, 3)
    | _, _ -> raise TypeError)
  | BinaryOp (e1, VectorAngle, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match t1, t2 with
    | TVector (TInt, dim1), TVector (TInt, dim2) -> if dim1 = dim2 then TFloat
    else raise TypeError
    | TVector (TFloat, dim1), TVector (TFloat, dim2) -> if dim1 = dim2 then TFloat
    else raise TypeError
    | _, _ -> raise TypeError)
  | BinaryOp (e1, VectorScale, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match t1, t2 with
    | TInt, TVector (TInt, dim) -> TVector (TInt, dim)
    | TFloat, TVector (TFloat, dim) -> TVector (TFloat, dim)
    | _, _ -> raise TypeError)
  | BinaryOp (e1, MatrixAdd, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match t1, t2 with
    | TMatrix (TInt, rows1, cols1), TMatrix (TInt, rows2, cols2) -> if rows1 = rows2 && cols1 = cols2 then TMatrix (TInt, rows1, cols1)
    else raise TypeError
    | TMatrix (TFloat, rows1, cols1), TMatrix (TFloat, rows2, cols2) -> if rows1 = rows2 && cols1 = cols2 then TMatrix (TFloat, rows1, cols1)
    else raise TypeError
    | _, _ -> raise TypeError)
  | BinaryOp (e1, MatrixMultiply, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match t1, t2 with
    | TMatrix (TInt, rows1, cols1), TMatrix (TInt, rows2, cols2) -> if cols1 = rows2 then TMatrix (TInt, rows1, cols2)
    else raise TypeError
    | TMatrix (TFloat, rows1, cols1), TMatrix (TFloat, rows2, cols2) -> if cols1 = rows2 then TMatrix (TFloat, rows1, cols2)
    else raise TypeError
    | _, _ -> raise TypeError)
  | BinaryOp (e1, MatrixScale, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match t1, t2 with
    | TInt, TMatrix (TInt, rows, cols) -> TMatrix (TInt, rows, cols)
    | TFloat, TMatrix (TFloat, rows, cols) -> TMatrix (TFloat, rows, cols)
    | _, _ -> raise TypeError)
  | BinaryOp (e1, MatrixVectorMultiply, e2) ->
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match t1, t2 with
    | TMatrix (TInt, rows, cols), TVector (TInt, dim) -> if cols = dim then TVector (TInt, rows)
    else raise TypeError
    | TMatrix (TFloat, rows, cols), TVector (TFloat, dim) -> if cols = dim then TVector (TFloat, rows)
    else raise TypeError
    | TVector (TInt, dim), TMatrix (TInt, rows, cols) -> if dim = rows then TVector (TInt, cols)
    else raise TypeError
    | TVector (TFloat, dim), TMatrix (TFloat, rows, cols) -> if dim = rows then TVector (TFloat, cols)
    else raise TypeError
    | _, _ -> raise TypeError)
  | UnaryOp (Not, e) ->
    let t = type_of e env in
    if t = TBool then TBool
    else raise TypeError
  | UnaryOp (Negate, e) ->
    let t = type_of e env in
    if t = TInt then TInt
    else if t = TFloat then TFloat
    else raise TypeError
  | UnaryOp (Abs, e) ->
    let t = type_of e env in
    if t = TInt then TInt
    else if t = TFloat then TFloat
    else raise TypeError
  | UnaryOp (AbsFloat, e) ->
    let t = type_of e env in
    if t = TFloat then TFloat
    else raise TypeError
  | UnaryOp (Sqrt, e) ->
    let t = type_of e env in
    if t = TInt then TFloat
    else if t = TFloat then TFloat
    else raise TypeError
  | UnaryOp (VectorMagnitude, e) ->
    let t = type_of e env in
    (match t with
    | TVector (TInt, _) -> TFloat
    | TVector (TFloat, _) -> TFloat
    | _ -> raise TypeError)
  | UnaryOp (VectorDimension, e) ->
    let t = type_of e env in
    (match t with
    | TVector (_, _dim) -> TInt
    | _ -> raise TypeError)
  | UnaryOp (VectorNormalize, e) ->
    let t = type_of e env in
    (match t with
    | TVector (TInt, dim) -> TVector (TFloat, dim)
    | TVector (TFloat, dim) -> TVector (TFloat, dim)
    | _ -> raise TypeError)
  | UnaryOp (MatrixRows, e) ->
    let t = type_of e env in
    (match t with
    | TMatrix (_, _rows, _) -> TInt
    | _ -> raise TypeError)
  | UnaryOp (MatrixCols, e) ->
    let t = type_of e env in
    (match t with
    | TMatrix (_, _, _cols) -> TInt
    | _ -> raise TypeError)
  | UnaryOp (MatrixDeterminant, e) ->
    let t = type_of e env in
    (match t with
    | TMatrix (TInt, rows, cols) -> if rows = cols then TInt
    else raise TypeError
    | TMatrix (TFloat, rows, cols) -> if rows = cols then TFloat
    else raise TypeError
    | _ -> raise TypeError)
  | UnaryOp (MatrixTranspose, e) ->
    let t = type_of e env in
    (match t with
    | TMatrix (TInt, rows, cols) -> TMatrix (TInt, cols, rows)
    | TMatrix (TFloat, rows, cols) -> TMatrix (TFloat, cols, rows)
    | _ -> raise TypeError)
  | UnaryOp (MatrixAdjoint, e) ->
    let t = type_of e env in
    (match t with
    | TMatrix (TInt, rows, cols) -> TMatrix (TInt, rows, cols)
    | TMatrix (TFloat, rows, cols) -> TMatrix (TFloat, rows, cols)
    | _ -> raise TypeError)
  | UnaryOp (MatrixInverse, e) ->
    let t = type_of e env in
    (match t with
    | TMatrix (TInt, rows, cols) -> TMatrix (TInt, rows, cols)
    | TMatrix (TFloat, rows, cols) -> TMatrix (TFloat, rows, cols)
    | _ -> raise TypeError)
  | UnaryOp (MatrixTrace, e) ->
    let t = type_of e env in
    (match t with
    | TMatrix (TInt, rows, cols) -> if rows = cols then TInt
    else raise TypeError
    | TMatrix (TFloat, rows, cols) -> if rows = cols then TFloat
    else raise TypeError
    | _ -> raise TypeError)
  | VectorElement (v, e) ->
    let t = type_of e env in
    let vec = lookup env v in
    (match vec with
    | TVector (TInt, _dim) -> if t = TInt then TInt
    else raise TypeError
    | TVector (TFloat, _dim) -> if t = TInt then TFloat
    else raise TypeError
    | _ -> raise TypeError)
  | MatrixElement (m, e1, e2) ->
    let mat = lookup env m in
    let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    (match mat with
    | TMatrix (TInt, _rows, _cols) -> if t1 = TInt && t2 = TInt then TInt
    else raise TypeError
    | TMatrix(TFloat, _rows, _cols) -> if t1 = TInt && t2 = TInt then TFloat
    else raise TypeError
    | _ -> raise TypeError)
  | Parenthesis e -> type_of e env
  | MatrixMinor (m, e1, e2) ->
    (let t1 = type_of e1 env in
    let t2 = type_of e2 env in
    let mat = lookup env m in

    (match mat with
    | TMatrix(TInt, rows, cols) -> if t1 = TInt && t2 = TInt then TMatrix(TInt, rows - 1, cols - 1)
    else raise TypeError
    | TMatrix(TFloat, rows, cols) -> if t1 = TInt && t2 = TInt then TMatrix(TFloat, rows - 1, cols - 1)
    else raise TypeError
    
    | _ -> raise TypeError)
    
    )
    
let comb_env env1 env2 =
  List.fold_left (fun acc (id, typ) -> if List.mem_assoc id acc then acc else (id, typ) :: acc) env1 env2
let rec type_stmt stmt env = match stmt with
Assign (id, e) -> let t = type_of e env in (id, t)::env
| Reassign_vector (id, e1, e2) ->
  (let typ = lookup env id in
  let t1 = type_of e1 env in
  let t2 = type_of e2 env in
  match typ with 
  TVector(TInt, _) -> if t1 = TInt && t2 = TInt then env else raise TypeError 
    | TVector(TFloat, _) -> if t1 = TInt && t2 = TFloat then env else raise TypeError
    | _ -> raise TypeError
  )
| Reassign_matrix (id, e1, e2, e3) ->
  (
    let typ = lookup env id in
    let t1 = type_of e1 env in
  let t2 = type_of e2 env in
  let t3 = type_of e3 env in
  match typ with
  TMatrix(TInt, _, _) -> if t1 = TInt && t2 = TInt && t3 = TInt then env else raise TypeError 
  | TMatrix(TFloat, _, _) -> if t1 = TInt && t2 = TInt && t3 = TFloat then env else raise TypeError 
  | _ -> raise TypeError
  ) 
| Expression e -> let _ = type_of e env in env
| If (cond, then_stmt, else_stmt) -> (let t = type_of cond env in
  if t = TBool then
    let env1 = type_stmt then_stmt env in
    let env2 = type_stmt else_stmt env in
    comb_env env1 env2
  else raise TypeError
    
  )
| While (cond, body) ->
  let t = type_of cond env in
  if t = TBool then 
    let new_env = type_stmt body env in
    new_env
  else raise TypeError
| For (e1, e2, e3, body) ->
  let t2 = type_of e2 env in
  let t3 = type_of e3 env in
  if t2 = TInt && t3 = TInt then
    let env' = (e1, TInt) :: env in
    type_stmt body env'
  else raise TypeError
| PrintIdentifier id -> let _ = lookup env id in env
| Block stmts -> List.fold_left (fun env stmt -> type_stmt stmt env) env stmts
| Input (types, id) -> (match types with
  "int" -> (id, TInt)::env
  | "float" -> (id, TFloat)::env
  | "bool" -> (id, TBool)::env
  | _ -> raise TypeError)
| Input_Vec (types, dim, id) ->( match types with
  "vec_int" -> (id, TVector (TInt, dim))::env
  | "vec_float" -> (id, TVector (TFloat, dim))::env
  | _ -> raise TypeError)
| Input_Mat (types, rows, cols, id) -> (match types with
  "mat_int" -> (id, TMatrix (TInt, rows, cols))::env
  | "mat_float" -> (id, TMatrix (TFloat, rows, cols))::env
  | _ -> raise TypeError)

| _ -> env

let type_program prog = 
  let _ = List.fold_left (fun env stmt -> type_stmt stmt env) [] prog in
  ()
