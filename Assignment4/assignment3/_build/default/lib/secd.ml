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

let rec compile e =
  match e with
  | Num n -> [ N n ]
  | B b -> [ BOOL b ]
  | V x -> [ LOOKUP x ]
  | Lam (x, e) -> [ MKCLOS (x, compile e @ [ RET ]) ]
  | App (e1, e2) -> compile e1 @ compile e2 @ [ APP ]
  | Add (e1, e2) -> compile e1 @ compile e2 @ [ ADD ]
  | Sub (e1, e2) -> compile e1 @ compile e2 @ [ SUB ]
  | Div (e1, e2) -> compile e1 @ compile e2 @ [ DIV ]
  | Mul (e1, e2) -> compile e1 @ compile e2 @ [ MUL ]
  | Mod (e1, e2) -> compile e1 @ compile e2 @ [ MOD ]
  | Exp (e1, e2) -> compile e1 @ compile e2 @ [ EXP ]
  | Eq (e1, e2) -> compile e1 @ compile e2 @ [ EQ ]
  | Neq (e1, e2) -> compile e1 @ compile e2 @ [ NEQ ]
  | Gt (e1, e2) -> compile e1 @ compile e2 @ [ GT ]
  | Ge (e1, e2) -> compile e1 @ compile e2 @ [ GE ]
  | Lt (e1, e2) -> compile e1 @ compile e2 @ [ LT ]
  | Le (e1, e2) -> compile e1 @ compile e2 @ [ LE ]
  | And (e1, e2) -> compile e1 @ compile e2 @ [ AND ]
  | Or (e1, e2) -> compile e1 @ compile e2 @ [ OR ]
  | Not e -> compile e @ [ NOT ]
  | Absolute e -> compile e @ [ ABSOLUTE ]
  | IfThenElse (e1, e2, e3) -> compile e1 @ [ COND (compile e2, compile e3) ]

let rec lookup x gamma =
  match gamma with
  | [] -> failwith ("unbound variable " ^ x)
  | (y, v) :: rest -> if x = y then v else lookup x rest

let rec secd_machine s e c d =
  match (s, e, c, d) with
  | x :: _s, _, [], _ -> x
  | s, e, N n :: c', d -> secd_machine (NANS n :: s) e c' d
  | s, e, BOOL b :: c', d -> secd_machine (BANS b :: s) e c' d
  | s, e, LOOKUP x :: c', d ->
      let v = lookup x e in
      secd_machine (v :: s) e c' d
  | s, e, MKCLOS (x, c1) :: c', d -> secd_machine (VClos (e, x, c1) :: s) e c' d
  | x :: VClos (e'', x'', c'') :: s', e', APP :: c', d ->
      secd_machine [] ((x'', x) :: e'') c'' ((s', e', c') :: d)
  | x :: _s', _e, RET :: _c', (s'', e'', c'') :: d' ->
      secd_machine (x :: s'') e'' c'' d'
  | NANS n1 :: NANS n2 :: s', e, ADD :: c', d ->
      secd_machine (NANS (n1 + n2) :: s') e c' d
  | NANS n1 :: NANS n2 :: s', e, SUB :: c', d ->
      secd_machine (NANS (n2 - n1) :: s') e c' d
  | NANS n1 :: NANS n2 :: s', e, MUL :: c', d ->
      secd_machine (NANS (n1 * n2) :: s') e c' d
  | NANS n1 :: NANS n2 :: s', e, DIV :: c', d ->
      if n1 = 0 then failwith "Division by zero"
      else secd_machine (NANS (n2 / n1) :: s') e c' d
  | NANS n1 :: NANS n2 :: s', e, MOD :: c', d ->
      if n1 = 0 then failwith "Modulo by zero"
      else secd_machine (NANS (n2 mod n1) :: s') e c' d
  | NANS n1 :: NANS n2 :: s', e, EXP :: c', d ->
      secd_machine
        (NANS (int_of_float (float_of_int n2 ** float_of_int n1)) :: s')
        e c' d
  | NANS n1 :: NANS n2 :: s', e, EQ :: c', d ->
      secd_machine (BANS (n1 = n2) :: s') e c' d
  | NANS n1 :: NANS n2 :: s', e, NEQ :: c', d ->
      secd_machine (BANS (n1 <> n2) :: s') e c' d
  | NANS n1 :: NANS n2 :: s', e, GT :: c', d ->
      secd_machine (BANS (n2 > n1) :: s') e c' d
  | NANS n1 :: NANS n2 :: s', e, GE :: c', d ->
      secd_machine (BANS (n2 >= n1) :: s') e c' d
  | NANS n1 :: NANS n2 :: s', e, LT :: c', d ->
      secd_machine (BANS (n2 < n1) :: s') e c' d
  | NANS n1 :: NANS n2 :: s', e, LE :: c', d ->
      secd_machine (BANS (n2 <= n1) :: s') e c' d
  | BANS b1 :: BANS b2 :: s', e, AND :: c', d ->
      secd_machine (BANS (b1 && b2) :: s') e c' d
  | BANS b1 :: BANS b2 :: s', e, OR :: c', d ->
      secd_machine (BANS (b1 || b2) :: s') e c' d
  | BANS b :: s', e, NOT :: c', d -> secd_machine (BANS (not b) :: s') e c' d
  | NANS n :: s', e, ABSOLUTE :: c', d ->
      secd_machine (NANS (abs n) :: s') e c' d
  | BANS b :: s', e, COND (c1, c2) :: c', d ->
      if b then secd_machine s' e (c1 @ c') d else secd_machine s' e (c2 @ c') d
  | _ -> failwith "Invalid stack state"

let run_secd e =
  let c = compile e in
  let s = [] in
  let e = [] in
  let d = [] in
  secd_machine s e c d

(* let lam1 = Lam ("w", App (V "a", V "b")) *)

let rec clos_to_string (c : answer) : string =
  match c with
  | NANS n -> string_of_int n
  | BANS b -> string_of_bool b
  | VClos (e, x, c) ->
      "Closure: " ^ x ^ " "
      ^ String.concat " "
          (List.map (fun (x, v) -> x ^ ": " ^ clos_to_string v) e)
      ^ String.concat " "
          (List.map
             (fun op ->
               match op with
               | N n -> string_of_int n
               | BOOL b -> string_of_bool b
               | LOOKUP x -> "(Lookup " ^ x ^ ")"
               | MKCLOS (x, _) -> x
               | APP -> "APP"
               | RET -> "RET"
               | ADD -> "ADD"
               | DIV -> "DIV"
               | MUL -> "MUL"
               | MOD -> "MOD"
               | EXP -> "EXP"
               | EQ -> "EQ"
               | NEQ -> "NEQ"
               | GT -> "GT"
               | GE -> "GE"
               | LT -> "LT"
               | LE -> "LE"
               | AND -> "AND"
               | OR -> "OR"
               | NOT -> "NOT"
               | ABSOLUTE -> "ABSOLUTE"
               | COND (_, _) -> "COND"
               | _ -> "UNKNOWN")
             c)

(* Test cases for the SECD machine *)

(* Simple arithmetic expressions *)
let test1 = Add (Num 3, Num 5) (* 3 + 5 *)
let test2 = Sub (Num 10, Num 4) (* 10 - 4 *)
let test3 = Mul (Num 6, Num 7) (* 6 * 7 *)
let test4 = Div (Num 20, Num 4) (* 20 / 4 *)
let test5 = Mod (Num 17, Num 5) (* 17 % 5 *)
let test6 = Exp (Num 2, Num 3) (* 2^3 *)

(* Boolean expressions *)
let test7 = And (B true, B false) (* true && false *)
let test8 = Or (B true, B false) (* true || false *)
let test9 = Not (B true) (* not true *)
let test10 = Eq (Num 5, Num 5) (* 5 = 5 *)
let test11 = Neq (Num 5, Num 3) (* 5 != 3 *)
let test12 = Gt (Num 7, Num 3) (* 7 > 3 *)
let test13 = Ge (Num 7, Num 7) (* 7 >= 7 *)
let test14 = Lt (Num 3, Num 7) (* 3 < 7 *)
let test15 = Le (Num 3, Num 3) (* 3 <= 3 *)

(* Absolute value *)
let test16 = Absolute (Num (-10)) (* abs(-10) *)

(* Conditional expressions *)
let test17 = IfThenElse (B true, Num 1, Num 0) (* if true then 1 else 0 *)
let test18 = IfThenElse (Eq (Num 5, Num 5), Num 10, Num 20)
(* if 5 = 5 then 10 else 20 *)

(* Lambda expressions and applications *)
let test19 = Lam ("x", Add (V "x", Num 1)) (* λx. x + 1 *)
let test20 = App (Lam ("x", V "x"), Num 5) (* (λx. x * 2) 5 *)

(* Nested lambda expressions *)
let test21 = Lam ("x", Lam ("y", Add (V "x", V "y"))) (* λx. λy. x + y *)
let test22 = App (App (Lam ("x", Lam ("y", Add (V "x", V "y"))), Num 3), Num 4)
(* ((λx. λy. x + y) 3) 4 *)

(* Complex expressions *)
let test23 =
  App
    ( Lam ("x", IfThenElse (Gt (V "x", Num 0), V "x", Absolute (V "x"))),
      Num (-5) )
(* (λx. if x > 0 then x else abs(x)) -5 *)

(* Running the tests *)
let test () =
  let tests =
    [
      test1;
      test2;
      test3;
      test4;
      test5;
      test6;
      test7;
      test8;
      test9;
      test10;
      test11;
      test12;
      test13;
      test14;
      test15;
      test16;
      test17;
      test18;
      test19;
      test20;
      test21;
      test22;
      test23;
    ]
  in
  List.iteri
    (fun i test ->
      Printf.printf "Test %d: %s\n" (i + 1) (clos_to_string (run_secd test)))
    tests

(* let lam1 = App (Lam ("x", Num 42), App (Lam ("y", V "y"), V "y")) *)

let lam2 =
  App
    ( Lam
        ( "x",
          IfThenElse
            ( Eq (V "x", Num 0),
              Num 1,
              App (Lam ("y", Div (Num 1, V "y")), Num 0) ) ),
      Num 0 )

(* let lam3 = App (Lam ("x", Num 10), App (Lam ("y", Div (V "y", Num 0)), Num 5)) *)

 let lam4 =
  App (Lam ("x", Add (V "x", Num 5)), App (Lam ("y", Mul (Num 2, V "y")), Num 3))

let lam5 =
  App
    ( Lam
        ( "x",
          IfThenElse
            ( Eq (V "x", Num 0),
              Num 1,
              App (Lam ("y", App (V "y", V "y")), Lam ("z", V "z")) ) ),
      Num 0 )

let () =
  test();
  (* Printf.printf "Test_cbn 1: %s\n" (clos_to_string (run_secd lam1)) *)
  Printf.printf "Test_cbn 2: %s\n" (clos_to_string (run_secd lam2));
 (* Printf.printf "Test_cbn 3: %s\n" (clos_to_string (run_secd lam3)) *)
 Printf.printf "Test_cbn 4: %s\n" (clos_to_string (run_secd lam4));
Printf.printf "Test_cbn 5: %s\n" (clos_to_string (run_secd lam5))
