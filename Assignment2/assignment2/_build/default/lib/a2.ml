type vector = float list
type types = Bool | Scalar | Vector of int
type values = B of bool | S of float | V of vector

exception DimensionError
exception TypeError

type expr =
  | T
  | F
  | ConstS of float
  | ConstV of vector
  | Add of expr * expr
  | Inv of expr
  | ScalProd of expr * expr
  | DotProd of expr * expr
  | Mag of expr
  | Angle of expr * expr
  | IsZero of expr
  | Cond of expr * expr * expr
  | UnitVector of expr
  | ZeroVector of expr
  | Sub of expr * expr

exception Wrong of expr

(* Though not used, this function was added for avoiding erros due to precision.*)
let _truncate x decimals =
  let multiplier = 10.0 ** float_of_int decimals in
  floor (x *. multiplier) /. multiplier

let create (n : int) (x : float) =
  if n < 1 then raise DimensionError
  else
    let rec create_n n acc =
      if n = 0 then acc else create_n (n - 1) (x :: acc)
    in
    create_n n []

let dim (v : vector) =
  let rec length u acc =
    match u with [] -> acc | _x :: xs -> length xs (acc + 1)
  in
  let len = length v 0 in
  if len < 1 then raise DimensionError else len

let is_zero (v : vector) =
  if dim v < 1 then raise DimensionError
  else
    let rec check_zero v =
      match v with
      | [] -> true
      | x :: xs -> if Float.abs x < 1e-6 then check_zero xs else false
    in
    check_zero v

let unit (n : int) (j : int) =
  if n < 1 || j < 1 || j > n then raise DimensionError
  else
    let rec gen_unit n j acc =
      if n = 0 then acc
      else gen_unit (n - 1) j ((if n = j then 1.0 else 0.0) :: acc)
    in
    gen_unit n j []

(* Here, I have used inbuilt function List.map, I could also do tail recursion here, but occam's razor. *)
let scale (x : float) (v : vector) =
  if dim v < 1 then raise DimensionError else List.map (fun y -> x *. y) v

(* In addv, I have added a helper function; to first check the dimension of arguments, if empty list was considered vector, I could have made addv itself recursive. *)
let addv (v1 : vector) (v2 : vector) =
  if dim v1 = dim v2 then
    let rec aux v1 v2 =
      match (v1, v2) with
      | [], [] -> []
      | x1 :: xs1, x2 :: xs2 -> (x1 +. x2) :: aux xs1 xs2
      | _ -> raise DimensionError
    in
    aux v1 v2
  else raise DimensionError

let dot_prod (v1 : vector) (v2 : vector) =
  if dim v1 = dim v2 then
    let rec aux v1 v2 acc =
      match (v1, v2) with
      | [], [] -> acc
      | x1 :: xs1, x2 :: xs2 -> aux xs1 xs2 (acc +. (x1 *. x2))
      | _ -> raise DimensionError
    in
    aux v1 v2 0.0
  else raise DimensionError

let inv (v : vector) =
  if dim v < 1 then raise DimensionError else List.map (fun x -> -.x) v

let length (v : vector) =
  if dim v < 1 then raise DimensionError else sqrt (dot_prod v v)

(* Because not specified which error should be raised in case of length of any vector is zero, I am raising dimension error. Zero division error could also be raised. *)
let angle (v1 : vector) (v2 : vector) =
  let len_v1 = length v1 in
  let len_v2 = length v2 in
  if len_v1 = 0.0 || len_v2 = 0.0 then raise DimensionError
  else acos (dot_prod v1 v2 /. (len_v1 *. len_v2))

let rec help_type_of (e : expr) =
  match e with
  | T -> Bool
  | F -> Bool
  | ConstS _ -> Scalar
  | ConstV v -> Vector (dim v)
  | Add (e1, e2) -> (
      match (help_type_of e1, help_type_of e2) with
      | Scalar, Scalar -> Scalar
      | Vector n, Vector m when n = m -> Vector n
      | Bool, Bool -> Bool
      | _ -> raise TypeError)
  | Inv e1 -> (
      match help_type_of e1 with
      | Scalar -> Scalar
      | Vector n -> Vector n
      | Bool -> Bool)
  | ScalProd (e1, e2) -> (
      match (help_type_of e1, help_type_of e2) with
      | Scalar, Scalar -> Scalar
      | Scalar, Vector n -> Vector n
      | Vector n, Scalar -> Vector n
      | Bool, Bool -> Bool
      | _ -> raise TypeError)
  | DotProd (e1, e2) -> (
      match (help_type_of e1, help_type_of e2) with
      | Vector n, Vector m when n = m -> Scalar
      | _ -> raise TypeError)
  | Mag e1 -> (
      match help_type_of e1 with
      | Vector _ -> Scalar
      | Scalar -> Scalar
      | _ -> raise TypeError)
  | Angle (e1, e2) -> (
      match (help_type_of e1, help_type_of e2) with
      | Vector n, Vector m when n = m -> Scalar
      | _ -> raise TypeError)
  | IsZero e1 -> (
      match help_type_of e1 with
      | Bool -> Bool
      | Scalar -> Bool
      | Vector _ -> Bool)
  | Cond (e1, e2, e3) -> (
      match (help_type_of e1, help_type_of e2, help_type_of e3) with
      | Bool, Bool, Bool -> Bool
      | Bool, Scalar, Scalar -> Scalar
      | Bool, Vector n, Vector m when n = m -> Vector n
      | _ -> raise TypeError)
  | UnitVector e1 -> (
      match help_type_of e1 with Vector n -> Vector n | _ -> raise TypeError)
  | ZeroVector e1 -> (
      match help_type_of e1 with Vector n -> Vector n | _ -> raise TypeError)
  | Sub (e1, e2) -> (
      match (help_type_of e1, help_type_of e2) with
      | Scalar, Scalar -> Scalar
      | Vector n, Vector m when n = m -> Vector n
      | _ -> raise TypeError)

let type_of (e : expr) = try help_type_of e with _ -> raise (Wrong e)

let rec help_eval (e : expr) =
  match e with
  | T -> B true
  | F -> B false
  | ConstS sca -> S sca
  | ConstV vec -> V vec
  | Add (e1, e2) -> (
      match (help_eval e1, help_eval e2) with
      | B true, B true -> B true
      | B false, B true -> B true
      | B false, B false -> B false
      | B true, B false -> B true
      | S sca1, S sca2 -> S (sca1 +. sca2)
      | V vec1, V vec2 -> V (addv vec1 vec2)
      | _ -> raise TypeError)
  | Inv e1 -> (
      match help_eval e1 with
      | B true -> B false
      | B false -> B true
      | S sca1 -> S (0.0 -. sca1)
      | V vec1 -> V (inv vec1))
  | ScalProd (e1, e2) -> (
      match (help_eval e1, help_eval e2) with
      | B false, B false -> B false
      | B false, B true -> B false
      | B true, B false -> B false
      | B true, B true -> B true
      | S sca1, V vec2 -> V (scale sca1 vec2)
      | V vec1, S sca2 -> V (scale sca2 vec1)
      | S sca1, S sca2 -> S (sca1 *. sca2)
      | _ -> raise TypeError)
  | DotProd (e1, e2) -> (
      match (help_eval e1, help_eval e2) with
      | V vec1, V vec2 -> S (dot_prod vec1 vec2)
      | _ -> raise TypeError)
  | Mag e1 -> (
      match help_eval e1 with
      | S sca -> S (Float.abs sca)
      | V vec -> S (length vec)
      | _ -> raise TypeError)
  | Angle (e1, e2) -> (
      match (help_eval e1, help_eval e2) with
      | V vec1, V vec2 -> S (angle vec1 vec2)
      | _ -> raise TypeError)
  | IsZero e1 -> (
      match help_eval e1 with
      | B false -> B true
      | B true -> B false
      | S sca1 -> B (if Float.abs sca1 < 1e-6 then true else false)
      | V vec -> B (if is_zero vec then true else false))
  | Cond (e1, e2, e3) -> (
      match help_eval e1 with
      | B true when type_of e2 = type_of e3 -> help_eval e2
      | B false when type_of e2 = type_of e3 -> help_eval e3
      | _ -> raise TypeError)
  | UnitVector e1 -> (
      match help_eval e1 with
      | V vec ->
          let len = length vec in
          if Float.abs len < 1e-6 then raise TypeError
          else V (scale (1.0 /. len) vec)
      | _ -> raise TypeError)
  | ZeroVector e1 -> (
      match help_eval e1 with
      | V vec -> V (create (dim vec) 0.0)
      | _ -> raise TypeError)
  | Sub (e1, e2) -> (
      match (help_eval e1, help_eval e2) with
      | S sca1, S sca2 -> S (sca1 -. sca2)
      | V vec1, V vec2 -> V (addv vec1 (inv vec2))
      | _ -> raise TypeError)

let eval (e : expr) = 
  let _ = type_of e in
  try help_eval e with _ -> raise (Wrong e)
