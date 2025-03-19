type myBool = T | F

type exp =
  | Num of int
  | B1 of myBool
  | V of string
  | Plus of exp * exp
  | Times of exp * exp
  | And of exp * exp
  | Or of exp * exp
  | Not of exp
  | Eq of exp * exp
  | Gt of exp * exp

type values = N of int | B of bool

type opcode =
  | LDN of int
  | LDB of myBool
  | Lookup of string
  | PLUS
  | TIMES
  | AND
  | OR
  | NOT
  | EQ
  | GT

exception Wrong

val myBool2bool : myBool -> bool
val size : exp -> int
val ht : exp -> int
val posttrav : exp -> string
val eval : exp -> (string -> values) -> values
val compile : exp -> opcode list
val stackmc : exp list -> opcode list -> (string -> exp) -> exp
val vars : exp -> string list
