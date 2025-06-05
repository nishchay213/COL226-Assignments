type variable = string

type lamexp =
  | V of variable
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

type opcode =
  | N of int
  | BOOL of bool
  | LOOKUP of variable
  | MKCLOS of string * opcode list
  | APP
  | RET
  | ADD
  | SUB
  | DIV
  | MUL
  | MOD
  | EXP
  | EQ
  | NEQ
  | GT
  | GE
  | LT
  | LE
  | AND
  | OR
  | NOT
  | ABSOLUTE
  | COND of opcode list * opcode list

type gamma = (string * answer) list
and answer = NANS of int | BANS of bool | VClos of gamma * string * code
and code = opcode list
and stack = answer list
and dump = (stack * environment * code) list
and environment = gamma

val test : unit -> unit
