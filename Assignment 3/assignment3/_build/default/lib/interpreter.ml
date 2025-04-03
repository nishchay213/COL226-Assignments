open Ast
exception RuntimeError of string

type values = Int of int | Float of float | Bool of bool | VectorInt of int list | VectorFloat of float list | MatrixInt of int list list | MatrixFloat of float list list

type env = {
  parent: env option;
  bindings: (string, values) Hashtbl.t;
}

let new_scope parent = {
  parent = Some parent;
  bindings = Hashtbl.create 8;
}

let global_scope () = {
  parent = None;
  bindings = Hashtbl.create 8;
}

let string_of_values v = 
  (match v with
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | VectorInt v -> 
      "[" ^ String.concat "; " (List.map string_of_int v) ^ "]"
  | VectorFloat v -> 
      "[" ^ String.concat "; " (List.map string_of_float v) ^ "]"
  | MatrixInt m ->
      "[" ^ String.concat "; " (List.map (fun row -> "[" ^ String.concat "; " (List.map string_of_int row) ^ "]") m) ^ "]"
  | MatrixFloat m ->
      "[" ^ String.concat "; " (List.map (fun row -> "[" ^ String.concat "; " (List.map string_of_float row) ^ "]") m) ^ "]")

let rec lookup id env =
  try Hashtbl.find env.bindings id
with Not_found ->
  match env.parent with
  | Some parent -> lookup id parent
  | None -> raise (RuntimeError ("Identifier " ^ id ^ " not found in environment"))


let transpose matrix =
    let rec transpose_aux acc = function
      | [] | [] :: _ -> List.rev acc
      | matrix -> transpose_aux (List.map List.hd matrix :: acc) (List.map List.tl matrix)
    in
    transpose_aux [] matrix

let rec determinant matrix =
  match matrix with
  | [[x]] -> x
  | _ ->
    List.mapi (fun i x ->
      let minor = List.tl (List.map (fun row -> List.filteri (fun j _ -> j <> i) row) matrix) in
      x *. determinant minor *. (if i mod 2 = 0 then 1.0 else -1.0)
    ) (List.hd matrix)
    |> List.fold_left (+.) 0.0
    
let cofactor matrix i j =
  let minor = List.filteri (fun x _ -> x <> i) matrix |> List.map (fun row -> List.filteri (fun y _ -> y <> j) row) in
  determinant minor *. (if (i + j) mod 2 = 0 then 1.0 else -1.0)

let adjoint matrix =
  List.mapi (fun i row ->
    List.mapi (fun j _ -> cofactor matrix i j) row
  ) matrix |> transpose
    
  

let rec interpret_expr expr env = match expr with
| Value (Int i) -> Int i
| Value (Float f) -> Float f
| Value (Bool b) -> Bool b
| Value (Identifier id) -> lookup id env
| Value (VectorInt (v, _dim)) -> VectorInt v
| Value (VectorFloat (v, _dim)) -> VectorFloat v
| Value (MatrixInt (m, _dim)) -> MatrixInt m
| Value (MatrixFloat (m, _dim)) -> MatrixFloat m
| UnaryOp (VectorDimension, e) ->
  let v = interpret_expr e env in
  (match v with
   | VectorInt v -> Int (List.length v)
   | VectorFloat v -> Int (List.length v)
   | _ -> raise (RuntimeError "Type error in dimension check"))
| BinaryOp (e1, Add, e2) -> 
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (Int i1, Int i2) -> Int (i1 + i2)
   | (Float f1, Float f2) -> Float (f1 +. f2)
   | _ -> raise (RuntimeError "Type error in addition"))
| BinaryOp (e1, Subtract, e2) -> 
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (Int i1, Int i2) -> Int (i1 - i2)
   | (Float f1, Float f2) -> Float (f1 -. f2)
   | _ -> raise (RuntimeError "Type error in subtraction"))
| BinaryOp (e1, Multiply, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (Int i1, Int i2) -> Int (i1 * i2)
   | (Float f1, Float f2) -> Float (f1 *. f2)
   | _ -> raise (RuntimeError "Type error in multiplication"))
| BinaryOp (e1, Divide, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (Int i1, Int i2) -> if i2 = 0 then raise (RuntimeError "Division by zero") else Int (i1 / i2)
   | (Float f1, Float f2) -> if f2 = 0.0 then raise (RuntimeError "Division by zero") else Float (f1 /. f2)
   | _ -> raise (RuntimeError "Type error in division"))
| BinaryOp (e1, Modulo, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (Int i1, Int i2) -> if i2 = 0 then raise (RuntimeError "Division by zero") else Int (i1 mod i2)
   | _ -> raise (RuntimeError "Type error in modulo"))
| BinaryOp (e1, Equal, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (Int i1, Int i2) -> Bool (i1 = i2)
   | (Float f1, Float f2) -> Bool (f1 = f2)
   | (Bool b1, Bool b2) -> Bool (b1 = b2)
   | (VectorInt v1, VectorInt v2) -> Bool (v1 = v2)
   | (VectorFloat v1, VectorFloat v2) -> Bool (v1 = v2)
   | (MatrixInt m1, MatrixInt m2) -> Bool (m1 = m2)
   | (MatrixFloat m1, MatrixFloat m2) -> Bool (m1 = m2)
   | _ -> raise (RuntimeError "Type error in equality check"))

| BinaryOp (e1, NotEqual, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (Int i1, Int i2) -> Bool (i1 <> i2)
   | (Float f1, Float f2) -> Bool (f1 <> f2)
   | (Bool b1, Bool b2) -> Bool (b1 <> b2)
   | (VectorInt v1, VectorInt v2) -> Bool (v1 <> v2)
   | (VectorFloat v1, VectorFloat v2) -> Bool (v1 <> v2)
   | (MatrixInt m1, MatrixInt m2) -> Bool (m1 <> m2)
   | (MatrixFloat m1, MatrixFloat m2) -> Bool (m1 <> m2)
   | _ -> raise (RuntimeError "Type error in inequality check"))
| BinaryOp (e1, Less, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (Int i1, Int i2) -> Bool (i1 < i2)
   | (Float f1, Float f2) -> Bool (f1 < f2)
   | _ -> raise (RuntimeError "Type error in less than check"))
| BinaryOp (e1, Greater, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (Int i1, Int i2) -> Bool (i1 > i2)
   | (Float f1, Float f2) -> Bool (f1 > f2)
   | _ -> raise (RuntimeError "Type error in greater than check"))
| BinaryOp (e1, LessEqual, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (Int i1, Int i2) -> Bool (i1 <= i2)
   | (Float f1, Float f2) -> Bool (f1 <= f2)
   | _ -> raise (RuntimeError "Type error in less than or equal check"))
| BinaryOp (e1, GreaterEqual, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (Int i1, Int i2) -> Bool (i1 >= i2)
   | (Float f1, Float f2) -> Bool (f1 >= f2)
   | _ -> raise (RuntimeError "Type error in greater than or equal check"))
| BinaryOp (e1, And, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (Bool b1, Bool b2) -> Bool (b1 && b2)
   | _ -> raise (RuntimeError "Type error in logical AND"))
| BinaryOp (e1, Or, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (Bool b1, Bool b2) -> Bool (b1 || b2)
   | _ -> raise (RuntimeError "Type error in logical OR"))
| BinaryOp (e1, Exponent, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (Int i1, Int i2) -> Float (((float_of_int i1) ** (float_of_int i2)))
   | (Float f1, Int f2) -> Float (f1 ** (float_of_int f2))
   | _ -> raise (RuntimeError "Type error in exponentiation"))
| BinaryOp (e1, AddFloat, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (Float f1, Float f2) -> Float (f1 +. f2)
   | _ -> raise (RuntimeError "Type error in float addition"))
| BinaryOp (e1, SubtractFloat, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (Float f1, Float f2) -> Float (f1 -. f2)
   | _ -> raise (RuntimeError "Type error in float subtraction"))
| BinaryOp (e1, MultiplyFloat, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (Float f1, Float f2) -> Float (f1 *. f2)
   | _ -> raise (RuntimeError "Type error in float multiplication"))
| BinaryOp (e1, DivideFloat, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (Float f1, Float f2) -> if f2 = 0.0 then raise (RuntimeError "Division by zero") else Float (f1 /. f2)
   | _ -> raise (RuntimeError "Type error in float division"))
| BinaryOp (e1, VectorAdd, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (VectorInt v1, VectorInt v2) -> 
       if List.length v1 <> List.length v2 then raise (RuntimeError "Vector size mismatch") else
       VectorInt (List.map2 (+) v1 v2)
   | (VectorFloat v1, VectorFloat v2) -> 
       if List.length v1 <> List.length v2 then raise (RuntimeError "Vector size mismatch") else
       VectorFloat (List.map2 (+.) v1 v2)
   | _ -> raise (RuntimeError "Type error in vector addition"))
| BinaryOp (e1, VectorDot, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (VectorInt v1, VectorInt v2) -> 
       if List.length v1 <> List.length v2 then raise (RuntimeError "Vector size mismatch") else
       Int (List.fold_left2 (fun acc x y -> acc + (x * y)) 0 v1 v2)
   | (VectorFloat v1, VectorFloat v2) -> 
       if List.length v1 <> List.length v2 then raise (RuntimeError "Vector size mismatch") else
       Float (List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0.0 v1 v2)
   | _ -> raise (RuntimeError "Type error in vector dot product"))
| BinaryOp (e1, VectorCross, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  let dim1 = match v1 with VectorInt v -> List.length v | VectorFloat v -> List.length v | _ -> raise (RuntimeError "Type error in vector cross product") in
  let dim2 = match v2 with VectorInt v -> List.length v | VectorFloat v -> List.length v | _ -> raise ( RuntimeError "Type error in vector cross product" )in
  if dim1 <> 3 || dim2 <> 3 then raise (RuntimeError "Vector cross product only defined for 3D vectors") else
  (match (v1, v2) with
  | (VectorInt [x1; y1; z1], VectorInt [x2; y2; z2]) ->
      VectorInt [y1 * z2 - z1 * y2; z1 * x2 - x1 * z2; x1 * y2 - y1 * x2]
  | (VectorFloat [x1; y1; z1], VectorFloat [x2; y2; z2]) ->
      VectorFloat [y1 *. z2 -. z1 *. y2; z1 *. x2 -. x1 *. z2; x1 *. y2 -. y1 *. x2]
  | _ -> raise (RuntimeError "Type error in vector cross product"))
| BinaryOp (e1, VectorAngle, e2) ->
  (let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  match (v1, v2) with
  | (VectorInt v1, VectorInt v2) -> 
      (if List.length v1 <> List.length v2 then raise (RuntimeError "Vector size mismatch") else
      let dot_product = List.fold_left2 (fun acc x y -> acc + (x * y)) 0 v1 v2 in
      let magnitude_v1 = sqrt (float_of_int (List.fold_left (fun acc x -> acc + (x * x)) 0 v1)) in
      let magnitude_v2 = sqrt (float_of_int (List.fold_left (fun acc x -> acc + (x * x)) 0 v2)) in
      try Float (acos ((float_of_int dot_product) /. (magnitude_v1 *. magnitude_v2)))
      with Division_by_zero -> raise (RuntimeError "Division by zero in vector angle calculation"))
  | (VectorFloat v1, VectorFloat v2) ->
      (if List.length v1 <> List.length v2 then raise (RuntimeError "Vector size mismatch") else
      let dot_product = List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0.0 v1 v2 in
      let magnitude_v1 = sqrt (List.fold_left (fun acc x -> acc +. (x *. x)) 0.0 v1) in
      let magnitude_v2 = sqrt (List.fold_left (fun acc x -> acc +. (x *. x)) 0.0 v2) in
      try Float (acos (dot_product /. (magnitude_v1 *. magnitude_v2)))
      with Division_by_zero -> raise (RuntimeError "Division by zero in vector angle calculation"))
      | _ -> raise (RuntimeError "Type error in vector angle calculation"))
| BinaryOp (e1, VectorScale, e2) ->
  (let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  match (v1, v2) with
  | (VectorInt v, Int s) -> VectorInt (List.map (fun x -> x * s) v)
  | (VectorFloat v, Float s) -> VectorFloat (List.map (fun x -> x *. s) v)
  | (Int s, VectorInt v) -> VectorInt (List.map (fun x -> x * s) v)
  | (Float s, VectorFloat v) -> VectorFloat (List.map (fun x -> x *. s) v)
  | _ -> raise (RuntimeError "Type error in vector scaling"))
| BinaryOp (e1, MatrixAdd, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (MatrixInt m1, MatrixInt m2) -> 
       if List.length m1 <> List.length m2 || List.length (List.hd m1) <> List.length (List.hd m2) then raise (RuntimeError "Matrix size mismatch") else
       MatrixInt (List.map2 (List.map2 (+)) m1 m2)
   | (MatrixFloat m1, MatrixFloat m2) -> 
       if List.length m1 <> List.length m2 || List.length (List.hd m1) <> List.length (List.hd m2) then raise (RuntimeError "Matrix size mismatch") else
       MatrixFloat (List.map2 (List.map2 (+.)) m1 m2)
   | _ -> raise (RuntimeError "Type error in matrix addition"))
| BinaryOp (e1, MatrixMultiply, e2) ->
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (MatrixInt m1, MatrixInt m2) -> 
       if List.length (List.hd m1) <> List.length m2 then raise (RuntimeError "Matrix size mismatch") else
       let m2_t = transpose m2 in
       let result = List.map (fun row ->
         List.map (fun col ->
           List.fold_left2 (fun acc x y -> acc + (x * y)) 0 row col
         ) m2_t
       ) m1 in
       MatrixInt result
   | (MatrixFloat m1, MatrixFloat m2) -> 
       if List.length (List.hd m1) <> List.length m2 then raise (RuntimeError "Matrix size mismatch") else
       let m2_t = transpose m2 in
       let result = List.map (fun row ->
         List.map (fun col ->
           List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0.0 row col
         ) m2_t
       ) m1 in
       MatrixFloat result
   | _ -> raise (RuntimeError "Type error in matrix multiplication"))
| BinaryOp (e1, MatrixScale, e2) -> 
  let v1 = interpret_expr e1 env in
  let v2 = interpret_expr e2 env in
  (match (v1, v2) with
   | (MatrixInt m, Int s) -> MatrixInt (List.map (List.map (( * ) s)) m)
   | (MatrixFloat m, Float s) -> MatrixFloat (List.map (List.map (( *. ) s)) m)
   | (Int s, MatrixInt m) -> MatrixInt (List.map (List.map (( * ) s)) m)
    | (Float s, MatrixFloat m) -> MatrixFloat (List.map (List.map (( *. ) s)) m)
   | _ -> raise (RuntimeError "Type error in matrix scaling"))
   | BinaryOp (e1, MatrixVectorMultiply, e2) ->
    let v1 = interpret_expr e1 env in
    let v2 = interpret_expr e2 env in
    (match (v1, v2) with
     (* Matrix * Vector multiplication *)
     | (MatrixInt m, VectorInt v) -> 
         if List.length (List.hd m) <> List.length v then 
           raise (RuntimeError "Matrix and vector size mismatch")
         else
           VectorInt (List.map (fun row -> List.fold_left2 (fun acc x y -> acc + (x * y)) 0 row v) m)

     | (MatrixFloat m, VectorFloat v) -> 
         if List.length (List.hd m) <> List.length v then 
           raise (RuntimeError "Matrix and vector size mismatch")
         else
           VectorFloat (List.map (fun row -> List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0.0 row v) m)

     (* Vector * Matrix multiplication *)
     | (VectorInt v, MatrixInt m) ->
         if List.length v <> List.length m then 
           raise (RuntimeError "Vector and matrix size mismatch")
         else
           VectorInt (
             List.map 
               (fun col -> List.fold_left2 (fun acc x y -> acc + (x * y)) 0 v col) 
               (transpose m)  (* Transpose the matrix to get columns *)
           )

     | (VectorFloat v, MatrixFloat m) ->
         if List.length v <> List.length m then 
           raise (RuntimeError "Vector and matrix size mismatch")
         else
           VectorFloat (
             List.map 
               (fun col -> List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0.0 v col) 
               (transpose m)  (* Transpose the matrix to get columns *)
           )

     | _ -> raise (RuntimeError "Type error in matrix-vector multiplication"))
  | UnaryOp (Not, e) ->
    let v = interpret_expr e env in
    (match v with
     | Bool b -> Bool (not b)
     | _ -> raise (RuntimeError "Type error in logical NOT"))
  | UnaryOp (Negate, e) ->
    let v = interpret_expr e env in
    (match v with
     | Int i -> Int (-i)
     | Float f -> Float (-.f)
     | _ -> raise (RuntimeError "Type error in negation"))
  | UnaryOp (Abs, e) ->
    let v = interpret_expr e env in
    (match v with
     | Int i -> Int (abs i)
     | Float f -> Float (abs_float f)
     | _ -> raise (RuntimeError "Type error in absolute value"))
  | UnaryOp (AbsFloat, e) ->
    let v = interpret_expr e env in
    (match v with
     | Float f -> Float (abs_float f)
     | _ -> raise (RuntimeError "Type error in absolute value for float"))
  | UnaryOp (Sqrt, e) ->
    let v = interpret_expr e env in
    (match v with
     | Int i -> if i < 0 then raise (RuntimeError "Square root of negative number") else Float (sqrt (float_of_int i))
     | Float f -> if f < 0.0 then raise (RuntimeError "Square root of negative number") else Float (sqrt f)
     | _ -> raise (RuntimeError "Type error in square root"))
  | UnaryOp (VectorMagnitude, e) ->
    let v = interpret_expr e env in
    (match v with
     | VectorInt v -> Float (sqrt (float_of_int (List.fold_left (fun acc x -> acc + (x * x)) 0 v)))
     | VectorFloat v -> Float (sqrt (List.fold_left (fun acc x -> acc +. (x *. x)) 0.0 v))
     | _ -> raise (RuntimeError "Type error in vector magnitude"))
  | UnaryOp (VectorNormalize, e) ->
    let v = interpret_expr e env in
    (match v with
     | VectorInt v -> 
         let mag = sqrt (float_of_int (List.fold_left (fun acc x -> acc + (x * x)) 0 v)) in
         if mag = 0.0 then raise (RuntimeError "Cannot normalize zero vector") else
         VectorFloat (List.map (fun x -> float_of_int x /. mag) v)
     | VectorFloat v -> 
         let mag = sqrt (List.fold_left (fun acc x -> acc +. (x *. x)) 0.0 v) in
         if mag = 0.0 then raise (RuntimeError "Cannot normalize zero vector") else
         VectorFloat (List.map (fun x -> x /. mag) v)
     | _ ->raise ( RuntimeError "Type error in vector normalization"))
  | UnaryOp (MatrixRows, e) ->
    let v = interpret_expr e env in
    (match v with
     | MatrixInt m -> Int (List.length m)
     | MatrixFloat m -> Int (List.length m)
     | _ -> raise  (RuntimeError "Type error in matrix rows"))
  | UnaryOp (MatrixCols, e) ->
    let v = interpret_expr e env in
    (match v with
     | MatrixInt m -> Int (List.length (List.hd m))
     | MatrixFloat m -> Int (List.length (List.hd m))
     | _ -> raise (RuntimeError "Type error in matrix columns"))
  | UnaryOp (MatrixDeterminant, e) ->
    let v = interpret_expr e env in
    
    (match v with
     | MatrixInt m -> 
         if List.length m = List.length (List.hd m) then 
           Float (determinant (List.map (List.map float_of_int) m))
         else raise (RuntimeError "Matrix must be square for determinant")
     | MatrixFloat m -> 
         if List.length m = List.length (List.hd m) then 
           Float (determinant m)
         else raise (RuntimeError "Matrix must be square for determinant")
     | _ -> raise (RuntimeError "Type error in matrix determinant"))
  | UnaryOp (MatrixTranspose, e) ->
    let v = interpret_expr e env in
    (match v with
     | MatrixInt m -> MatrixInt (transpose m)
     | MatrixFloat m -> MatrixFloat (transpose m)
     | _ -> raise (RuntimeError "Type error in matrix transpose"))
  | UnaryOp (MatrixAdjoint, e) ->
    let v = interpret_expr e env in
    
    (match v with
     | MatrixInt m -> 
         if List.length m = List.length (List.hd m) then 
           MatrixFloat (adjoint (List.map (List.map float_of_int) m))
         else raise (RuntimeError "Matrix must be square for adjoint")
     | MatrixFloat m -> 
         if List.length m = List.length (List.hd m) then 
           MatrixFloat (adjoint m)
         else raise (RuntimeError "Matrix must be square for adjoint")
     | _ -> raise (RuntimeError "Type error in matrix adjoint"))
  | UnaryOp (MatrixInverse, e) ->
    let v = interpret_expr e env in
    let scalar_multiply matrix scalar =
      List.map (fun row -> List.map (fun x -> x *. scalar) row) matrix
    in
    (match v with
      | MatrixInt m ->
        if List.length m = List.length (List.hd m) then
          let det = determinant (List.map (List.map float_of_int) m) in
          if det = 0.0 then raise (RuntimeError "Matrix is singular and cannot be inverted")
          else MatrixFloat (scalar_multiply (adjoint (List.map (List.map float_of_int) m)) (1.0 /. det))
        else raise (RuntimeError "Matrix must be square for inversion")
     | MatrixFloat m ->
       if List.length m = List.length (List.hd m) then
         let det = determinant m in
         if det = 0.0 then raise (RuntimeError "Matrix is singular and cannot be inverted")
         else MatrixFloat (scalar_multiply (adjoint m) (1.0 /. det))
       else raise (RuntimeError "Matrix must be square for inversion")
     | _ -> raise (RuntimeError "Type error in matrix inversion"))
  | UnaryOp (MatrixTrace, e) ->
    let v = interpret_expr e env in
    (match v with
     | MatrixInt m -> if List.length m = List.length (List.hd m) then Int (List.fold_left (fun acc i -> acc + List.nth (List.nth m i) i) 0 (List.init (List.length m) (fun x -> x)))
                      else raise (RuntimeError "Matrix must be square for trace")
     | MatrixFloat m -> if List.length m = List.length (List.hd m) then Float (List.fold_left (fun acc i -> acc +. List.nth (List.nth m i) i) 0.0 (List.init (List.length m) (fun x -> x)))
                        else raise (RuntimeError "Matrix must be square for trace")
     | _ -> raise (RuntimeError "Type error in matrix trace"))
  | MatrixMinor (m, e1, e2) ->
   ( let val1 = interpret_expr e1 env in
    let val2 = interpret_expr e2 env in
    let matrix = lookup m env in
    match (val1, val2, matrix) with
    | (Int i, Int j, MatrixInt mat) ->
        if i < 0 || i >= List.length mat || j < 0 || j >= List.length (List.hd mat) then raise (RuntimeError "Index out of bounds") else
        let minor = List.filteri (fun x _ -> x <> i) mat |> List.map (fun row -> List.filteri (fun y _ -> y <> j) row) in
        MatrixInt minor
    | (Int i, Int j, MatrixFloat mat) ->
        if i < 0 || i >= List.length mat || j < 0 || j >= List.length (List.hd mat) then raise (RuntimeError "Index out of bounds") else
        let minor = List.filteri (fun x _ -> x <> i) mat |> List.map (fun row -> List.filteri (fun y _ -> y <> j) row) in
        MatrixFloat minor
    | _ -> raise (RuntimeError "Type error in matrix minor"))
  | Parenthesis e -> interpret_expr e env
  | VectorElement (e1, e2) ->
    (let v1 = lookup e1 env in
    let v2 = interpret_expr e2 env in
    match (v1, v2) with
     | (VectorInt v, Int i) -> if i < 0 || i >= List.length v then raise (RuntimeError "Index out of bounds") else Int (List.nth v i)
     | (VectorFloat v, Int i) -> if i < 0 || i >= List.length v then raise (RuntimeError "Index out of bounds") else Float (List.nth v i)
     | _ -> raise (RuntimeError "Type error in vector indexing"))
  | MatrixElement (e1, e2, e3) ->
    (let v1 = lookup e1 env in
    let v2 = interpret_expr e2 env in
    let v3 = interpret_expr e3 env in
    match (v1, v2, v3) with
     | (MatrixInt m, Int i, Int j) -> if i < 0 || i >= List.length m || j < 0 || j >= List.length (List.hd m) then raise (RuntimeError "Index out of bounds") else Int (List.nth (List.nth m i) j)
     | (MatrixFloat m, Int i, Int j) -> if i < 0 || i >= List.length m || j < 0 || j >= List.length (List.hd m) then raise (RuntimeError "Index out of bounds") else Float (List.nth (List.nth m i) j)
     | _ -> raise (RuntimeError "Type error in matrix indexing"))
  
    




          


let rec update_env id value env =
  if Hashtbl.mem env.bindings id then
    (Hashtbl.replace env.bindings id value; env)
  else
    match env.parent with
    | Some parent -> 
        let parent' = update_env id value parent in
        { env with parent = Some parent' }
    | None -> raise (RuntimeError ("Identifier " ^ id ^ " not found in any scope"))

let assign id value env =
  if Hashtbl.mem env.bindings id then
    raise (RuntimeError ("Identifier " ^ id ^ " already exists in the current scope"))
  else
    (Hashtbl.replace env.bindings id value; env)

let rec interpret_statement stmt env = match stmt with
| Reassign (id, expr) ->
  let value = interpret_expr expr env in
  update_env id value env
| Assign (_, id, e) ->
  let value = interpret_expr e env in
  assign id value env
  
| Reassign_vector (id, index, expr) ->
 ( let ind = interpret_expr index env in
  let value = interpret_expr expr env in
  match ind with
  | Int i ->
      let v = lookup id env in
      (match v with
       | VectorInt vec -> if i < 0 || i >= List.length vec then raise (RuntimeError "Index out of bounds") else
            let value_int = match value with Int i -> i | _ -> raise (RuntimeError "Type error in vector reassignment") in
           let new_vec = List.mapi (fun j x -> if j = i then value_int else x) vec in
           update_env id (VectorInt new_vec) env
       | VectorFloat vec -> if i < 0 || i >= List.length vec then raise (RuntimeError "Index out of bounds") else
          let value_float = match value with Float f -> f | _ -> raise (RuntimeError "Type error in vector reassignment") in
           let new_vec = List.mapi (fun j x -> if j = i then value_float else x) vec in

            update_env id (VectorFloat new_vec) env
       | _ -> raise (RuntimeError "Type error in vector reassignment"))
  | _ -> raise (RuntimeError "Index must be an integer"))

| Reassign_matrix (id, row, col, expr) ->
  (let r = interpret_expr row env in
  let c = interpret_expr col env in
  let value = interpret_expr expr env in
  match (r, c) with
  | (Int i, Int j) ->
      let m = lookup id env in
      (match m with
       | MatrixInt mat -> if i < 0 || i >= List.length mat || j < 0 || j >= List.length (List.hd mat) then raise (RuntimeError "Index out of bounds") else
            let value_int = match value with Int i -> i | _ -> raise (RuntimeError "Type error in matrix reassignment") in
           let new_mat = List.mapi (fun x row -> if x = i then List.mapi (fun y v -> if y = j then value_int else v) row else row) mat in
           update_env id (MatrixInt new_mat) env
       | MatrixFloat mat -> if i < 0 || i >= List.length mat || j < 0 || j >= List.length (List.hd mat) then raise (RuntimeError "Index out of bounds") else
            let value_float = match value with Float f -> f | _ -> raise (RuntimeError "Type error in matrix reassignment") in
           
           let new_mat = List.mapi (fun x row -> if x = i then List.mapi (fun y v -> if y = j then value_float else v) row else row) mat in
            update_env id (MatrixFloat new_mat) env
       | _ -> raise (RuntimeError "Type error in matrix reassignment"))
  | _ -> raise (RuntimeError "Row and column indices must be integers"))
  

| Expression expr ->
  let _ = interpret_expr expr env in
  env
| If (cond, then_stmt, else_stmt) ->
  let cond_value = interpret_expr cond env in
  (match cond_value with
   | Bool true -> let new_env = new_scope env in let _ = interpret_statement then_stmt new_env in env
   | Bool false -> let new_env = new_scope env in let _ = interpret_statement else_stmt new_env in env
   | _ -> raise (RuntimeError "Condition must be a boolean expression"))
| While (cond, body) ->
  let rec loop env =
    let cond_value = interpret_expr cond env in
    match cond_value with
    | Bool true ->
        (* let new_env = new_scope env in *)
        let new_env = interpret_statement body env in
        loop new_env
    | Bool false -> env
    | _ -> raise (RuntimeError "Condition must be a boolean expression")
  in loop env
| For (id, init_expr, cond_expr, body) ->
  (let init_value = interpret_expr init_expr env in
  let fin_value = interpret_expr cond_expr env in
  match (init_value, fin_value) with
  | (Int start, Int finish) ->
      if (start > finish) then
        let rec loop i env =
          if i <= finish then env
          else
            let new_env = new_scope env in
            Hashtbl.replace new_env.bindings id (Int i);
            let _ = interpret_statement body new_env in
            loop (i-1) env in
            loop start env
      else
        let rec loop i env =
          if i >= finish then env
          else
            let new_env = new_scope env in
            Hashtbl.replace new_env.bindings id (Int i);
            let _ = interpret_statement body new_env in
            loop (i+1) env in 
          loop start env
  | _ -> raise (RuntimeError "For loop requires integer expressions"))

| Block stmts ->
  let new_env = new_scope env in
  let _ = List.fold_left (fun e stmt -> interpret_statement stmt e) new_env stmts in
  env
  

| PrintIdentifier id ->
  let value = lookup id env in
  Printf.printf "%s\n" (string_of_values value); env
| Input (types, id) -> 
  let input_value = 
    match types with
    | "int" -> Int (read_int ())
    | "float" -> Float (read_float ())
    | "bool" -> Bool (read_line () = "T")
    | _ -> raise (RuntimeError "Unsupported input type")
  in
  Hashtbl.replace env.bindings id input_value; env
| Input_Vec (types, dim, id) -> (match types with
  | "vec_int" -> 
      
      let input_line = read_line () in
      let values = String.split_on_char ' ' input_line in
      if List.length values < dim then
        raise (RuntimeError "Not enough values provided for vector")
      else
        let vec = List.map int_of_string (List.filteri (fun i _ -> i < dim) values) in
        Hashtbl.replace env.bindings id (VectorInt(vec)); env
  | "vec_float" ->
      
      let input_line = read_line () in
      let values = String.split_on_char ' ' input_line in
      if List.length values < dim then
        raise (RuntimeError "Not enough values provided for vector")
      else
        let vec = List.map float_of_string (List.filteri (fun i _ -> i < dim) values) in
        Hashtbl.replace env.bindings id (VectorFloat(vec)); env
 
  | _ -> raise (RuntimeError "Unsupported vector input type"))

| Input_Mat (types, dim1, dim2, id) -> (match types with
  | "mat_int" -> 
      let input_lines = List.init dim1 (fun _ -> read_line ()) in
      let values = List.map (String.split_on_char ' ') input_lines in
      if List.length values < dim1 then
       raise ( RuntimeError "Not enough rows provided for matrix")
      else
        let mat = List.map (List.filteri (fun i _ -> i < dim2)) values in
        let mat_int = List.map (List.map int_of_string) mat in
        Hashtbl.replace env.bindings id (MatrixInt(mat_int)); env
  | "mat_float" ->
      let input_lines = List.init dim1 (fun _ -> read_line ()) in
      let values = List.map (String.split_on_char ' ') input_lines in
      if List.length values < dim1 then
        raise (RuntimeError "Not enough rows provided for matrix")
      else
        let mat = List.map (List.filteri (fun i _ -> i < dim2)) values in
        let mat_float = List.map (List.map float_of_string) mat in
        Hashtbl.replace env.bindings id (MatrixFloat(mat_float)); env

  | _ ->raise ( RuntimeError "Unsupported matrix input type")  )
  
| InputFile (types, id, filename) ->
  (let input_file = open_in filename in
  try
    let line = input_line input_file in
    close_in input_file;
    match types with
    | "int" -> let value = int_of_string line in
        assign id (Int value) env
    | "float" -> let value = float_of_string line in
        assign id (Float value) env
    | "bool" -> let value = if line = "T" then Bool true else Bool false in
        assign id value env
    | _ -> raise (RuntimeError "Unsupported file input type")
  with
  | End_of_file -> 
      close_in input_file;
      raise (RuntimeError "File is empty")
  | e -> 
      close_in_noerr input_file;
      raise e)
| InputFile_Vec (types, dim, id, filename) ->
  (let input_file = open_in filename in
  try
    let line = input_line input_file in
    close_in input_file;
    match types with
    | "vec_int" -> 
        let values = String.split_on_char ' ' line in
        if List.length values < dim then raise (RuntimeError "Not enough values provided for vector") else
        let vec = List.map int_of_string (List.filteri (fun i _ -> i < dim) values) in
        assign id (VectorInt(vec)) env
    | "vec_float" -> 
        let values = String.split_on_char ' ' line in
        if List.length values < dim then raise (RuntimeError "Not enough values provided for vector") else
        let vec = List.map float_of_string (List.filteri (fun i _ -> i < dim) values) in
        assign id (VectorFloat(vec)) env

    | _ -> raise (RuntimeError "Unsupported file vector input type")
  with
  | End_of_file -> 
      close_in input_file;
      raise (RuntimeError "File is empty")
  | e -> 
      close_in_noerr input_file;
      raise e)
| InputFile_Mat (types, dim1, dim2, id, filename) ->
  (let input_file = open_in filename in
  try
    let lines = List.init dim1 (fun _ -> input_line input_file) in
    close_in input_file;
    match types with
    | "mat_int" -> 
        let values = List.map (String.split_on_char ' ') lines in
        if List.length values < dim1 then raise (RuntimeError "Not enough rows provided for matrix") else
        let mat = List.map (List.filteri (fun i _ -> i < dim2)) values in
        let mat_int = List.map (List.map int_of_string) mat in
        assign id (MatrixInt(mat_int)) env
    | "mat_float" -> 
        let values = List.map (String.split_on_char ' ') lines in
        if List.length values < dim1 then raise (RuntimeError "Not enough rows provided for matrix") else
        let mat = List.map (List.filteri (fun i _ -> i < dim2)) values in
        let mat_float = List.map (List.map float_of_string) mat in
        assign id (MatrixFloat(mat_float)) env

    | _ -> raise (RuntimeError "Unsupported file matrix input type")
  with
  | End_of_file -> 
      close_in input_file;
      raise (RuntimeError "File is empty")
  | e -> 
      close_in_noerr input_file;
      raise e)
| Raise e -> raise (RuntimeError (Printf.sprintf "Runtime error: %s" e))
| _ -> raise (RuntimeError "Unsupported statement type")


let interpret_program prog =
  let env = global_scope () in
  let _ = List.fold_left (fun e stmt -> interpret_statement stmt e) env prog in
  ()