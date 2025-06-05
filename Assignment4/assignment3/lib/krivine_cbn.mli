val test : unit -> unit

type variable = string

type lamexp =
  | V of variable
  | UBV of variable
  | App of lamexp * lamexp
  | Lam of variable * lamexp
  | Num of int
  | B of bool
  | Add of lamexp * lamexp
  | Sub of lamexp * lamexp
  | Div of lamexp * lamexp
  | Mul of lamexp * lamexp
  | Mod of lamexp * lamexp
  | Exp of lamexp * lamexp
  | Eq of lamexp * lamexp
  | Neq of lamexp * lamexp
  | Gt of lamexp * lamexp
  | Ge of lamexp * lamexp
  | Lt of lamexp * lamexp
  | Le of lamexp * lamexp
  | And of lamexp * lamexp
  | Or of lamexp * lamexp
  | Not of lamexp
  | Absolute of lamexp
  | IfThenElse of lamexp * lamexp * lamexp

type closure = Clos of lamexp * gamma
and gamma = (lamexp * closure) list
