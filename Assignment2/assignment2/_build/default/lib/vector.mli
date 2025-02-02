type vector = float list
exception DimensionError
exception TypeError
type types = Bool | Scalar | Vector of int
type myBool = T | F
type values = B of myBool | S of float | V of vector

type expr = T | F
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
;;

exception Wrong of expr


val create : int -> float -> vector
val dim : vector -> int
val is_zero : vector -> bool
val unit : int -> int -> vector
val scale : float -> vector -> vector
val addv : vector -> vector -> vector
val dot_prod : vector -> vector -> float
val inv : vector -> vector
val length : vector -> float
val angle : vector -> vector -> float
val type_of : expr -> types
val eval : expr -> values
