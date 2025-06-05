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

type closure = Clos of lamexp * gamma
and gamma = (lamexp * closure) list

let rec lookup e gamma =
  match gamma with
  | (e1, cl1) :: gamma' ->
      if e = e1 then
        match cl1 with
        | Clos (Lam (x, e2), gamma) -> Clos (Lam (x, e2), (e1, cl1) :: gamma)
        | _ -> cl1
      else lookup e gamma'
  | [] ->
      let var = match e with V x -> x | _ -> failwith "e is not variable" in
      failwith ("unbound variable " ^ var)

let rec krivine_machine cl s =
  match cl with
  | Clos (Num n, gamma) -> Clos (Num n, gamma)
  | Clos (B b, gamma) -> Clos (B b, gamma)
  | Clos (V x, gamma) -> krivine_machine (lookup (V x) gamma) s
  | Clos (Lam (x, e), gamma) ->
      let new_cl, new_s =
        match s with
        | cl1 :: s' -> (Clos (e, (V x, cl1) :: gamma), s')
        | [] -> failwith "Invalid state: no closure in stack"
      in

      krivine_machine new_cl new_s
  | Clos (App (e1, e2), gamma) ->
      krivine_machine (Clos (e1, gamma)) (Clos (e2, gamma) :: s)
  | Clos (Add (e1, e2), gamma) ->
      let cl1 = krivine_machine (Clos (e1, gamma)) [] in
      let cl2 = krivine_machine (Clos (e2, gamma)) [] in
      let new_clos =
        match (cl1, cl2) with
        | Clos (Num n1, _), Clos (Num n2, _) -> Clos (Num (n1 + n2), [])
        | _ -> failwith "Addition failed"
      in
      krivine_machine new_clos s
  | Clos (Sub (e1, e2), gamma) ->
      let cl1 = krivine_machine (Clos (e1, gamma)) [] in
      let cl2 = krivine_machine (Clos (e2, gamma)) [] in
      let new_clos =
        match (cl1, cl2) with
        | Clos (Num n1, _), Clos (Num n2, _) -> Clos (Num (n1 - n2), [])
        | _ -> failwith "Subtraction failed"
      in
      krivine_machine new_clos s
  | Clos (Mul (e1, e2), gamma) ->
      let cl1 = krivine_machine (Clos (e1, gamma)) [] in
      let cl2 = krivine_machine (Clos (e2, gamma)) [] in
      let new_clos =
        match (cl1, cl2) with
        | Clos (Num n1, _), Clos (Num n2, _) -> Clos (Num (n1 * n2), [])
        | _ -> failwith "Multiplication failed"
      in
      krivine_machine new_clos s
  | Clos (Div (e1, e2), gamma) ->
      let cl1 = krivine_machine (Clos (e1, gamma)) [] in
      let cl2 = krivine_machine (Clos (e2, gamma)) [] in
      let new_clos =
        match (cl1, cl2) with
        | Clos (Num n1, _), Clos (Num n2, _) ->
            if n2 = 0 then failwith "Division by zero"
            else Clos (Num (n1 / n2), [])
        | _ -> failwith "Division failed"
      in
      krivine_machine new_clos s
  | Clos (Mod (e1, e2), gamma) ->
      let cl1 = krivine_machine (Clos (e1, gamma)) [] in

      let cl2 = krivine_machine (Clos (e2, gamma)) [] in
      let new_clos =
        match (cl1, cl2) with
        | Clos (Num n1, _), Clos (Num n2, _) ->
            if n2 = 0 then failwith "Modulo by zero"
            else Clos (Num (n1 mod n2), [])
        | _ -> failwith "Modulus failed"
      in
      krivine_machine new_clos s
  | Clos (Exp (e1, e2), gamma) ->
      let cl1 = krivine_machine (Clos (e1, gamma)) [] in

      let cl2 = krivine_machine (Clos (e2, gamma)) [] in
      let new_clos =
        match (cl1, cl2) with
        | Clos (Num n1, _), Clos (Num n2, _) ->
            Clos (Num (int_of_float (float_of_int n1 ** float_of_int n2)), [])
        | _ -> failwith "Exponentiation failed"
      in
      krivine_machine new_clos s
  | Clos (Eq (e1, e2), gamma) ->
      let cl1 = krivine_machine (Clos (e1, gamma)) [] in

      let cl2 = krivine_machine (Clos (e2, gamma)) [] in
      let new_clos =
        match (cl1, cl2) with
        | Clos (Num n1, _), Clos (Num n2, _) -> Clos (B (n1 = n2), [])
        | _ -> failwith "Equality check failed"
      in
      krivine_machine new_clos s
  | Clos (Neq (e1, e2), gamma) ->
      let cl1 = krivine_machine (Clos (e1, gamma)) [] in

      let cl2 = krivine_machine (Clos (e2, gamma)) [] in
      let new_clos =
        match (cl1, cl2) with
        | Clos (Num n1, _), Clos (Num n2, _) -> Clos (B (n1 <> n2), [])
        | _ -> failwith "Inequality check failed"
      in
      krivine_machine new_clos s
  | Clos (Gt (e1, e2), gamma) ->
      let cl1 = krivine_machine (Clos (e1, gamma)) [] in

      let cl2 = krivine_machine (Clos (e2, gamma)) [] in
      let new_clos =
        match (cl1, cl2) with
        | Clos (Num n1, _), Clos (Num n2, _) -> Clos (B (n1 > n2), [])
        | _ -> failwith "Greater than check failed"
      in
      krivine_machine new_clos s
  | Clos (Ge (e1, e2), gamma) ->
      let cl1 = krivine_machine (Clos (e1, gamma)) [] in

      let cl2 = krivine_machine (Clos (e2, gamma)) [] in
      let new_clos =
        match (cl1, cl2) with
        | Clos (Num n1, _), Clos (Num n2, _) -> Clos (B (n1 >= n2), [])
        | _ -> failwith "Greater than or equal check failed"
      in
      krivine_machine new_clos s
  | Clos (Lt (e1, e2), gamma) ->
      let cl1 = krivine_machine (Clos (e1, gamma)) [] in

      let cl2 = krivine_machine (Clos (e2, gamma)) [] in
      let new_clos =
        match (cl1, cl2) with
        | Clos (Num n1, _), Clos (Num n2, _) -> Clos (B (n1 < n2), [])
        | _ -> failwith "Less than check failed"
      in
      krivine_machine new_clos s
  | Clos (Le (e1, e2), gamma) ->
      let cl1 = krivine_machine (Clos (e1, gamma)) [] in

      let cl2 = krivine_machine (Clos (e2, gamma)) [] in
      let new_clos =
        match (cl1, cl2) with
        | Clos (Num n1, _), Clos (Num n2, _) -> Clos (B (n1 <= n2), [])
        | _ -> failwith "Less than or equal check failed"
      in
      krivine_machine new_clos s
  | Clos (And (e1, e2), gamma) ->
      let cl1 = krivine_machine (Clos (e1, gamma)) [] in

      let cl2 = krivine_machine (Clos (e2, gamma)) [] in
      let new_clos =
        match (cl1, cl2) with
        | Clos (B b1, _), Clos (B b2, _) -> Clos (B (b1 && b2), [])
        | _ -> failwith "Logical AND check failed"
      in
      krivine_machine new_clos s
  | Clos (Or (e1, e2), gamma) ->
      let cl1 = krivine_machine (Clos (e1, gamma)) [] in

      let cl2 = krivine_machine (Clos (e2, gamma)) [] in
      let new_clos =
        match (cl1, cl2) with
        | Clos (B b1, _), Clos (B b2, _) -> Clos (B (b1 || b2), [])
        | _ -> failwith "Logical OR check failed"
      in
      krivine_machine new_clos s
  | Clos (Not e, gamma) ->
      let cl1 = krivine_machine (Clos (e, gamma)) [] in

      let new_clos =
        match cl1 with
        | Clos (B b, _) -> Clos (B (not b), [])
        | _ -> failwith "Logical NOT check failed"
      in
      krivine_machine new_clos s
  | Clos (IfThenElse (e1, e2, e3), gamma) ->
      let cl1 = krivine_machine (Clos (e1, gamma)) [] in

      let new_clos =
        match cl1 with
        | Clos (B b, _) ->
            if b then krivine_machine (Clos (e2, gamma)) s
            else krivine_machine (Clos (e3, gamma)) s
        | _ -> failwith "IfThenElse check failed"
      in
      krivine_machine new_clos s
  | Clos (Absolute e, gamma) ->
      let cl1 = krivine_machine (Clos (e, gamma)) [] in

      let new_clos =
        match cl1 with
        | Clos (Num n, _) -> Clos (Num (abs n), [])
        | _ -> failwith "Absolute check failed"
      in
      krivine_machine new_clos s

let unload e gamma =
  let cl = krivine_machine (Clos (e, gamma)) [] in
  match cl with
  | Clos (Num n, _) -> Num n
  | Clos (B b, _) -> B b
  | Clos (V x, _) -> V x
  | Clos (Lam (x, e), _) -> Lam (x, e)
  | _ -> failwith "Unexpected application in result"

(* let lam1 = App (Lam ("x", App (Lam ("y", Add (V "x", V "y")), Num 33)), Num 5) *)
(* let lam2 = Lam ("x", Lam ("y", App (V "x", V "y")))
let lam3 = Lam ("x", App (V "x", V "x")) *)

(* let test1 = (unload lam1 []) = V "y" *)

let rec lamexp_to_string lamexp =
  match lamexp with
  | Num n -> string_of_int n
  | B b -> ( match b with true -> "true" | _ -> "false")
  | V x -> x
  | Lam (x, e) -> Printf.sprintf "(\\%s -> %s)" x (lamexp_to_string e)
  | _ -> "not supported yet"

let lam1 = App (Lam ("x", Add (V "x", Num 10)), Num 5)
let lam2 = App (Lam ("x", App (Lam ("y", Add (V "x", V "y")), Num 13)), Num 7)
let lam3 = App (Lam ("x", Lam ("y", Add (V "x", V "y"))), Num 5)
let lam4 = App (Lam ("x", Lam ("y", Sub (V "x", V "y"))), Num 10)
let lam5 = App (Lam ("x", Lam ("y", Mul (V "x", V "y"))), Num 4)
let lam6 = App (Lam ("x", Lam ("y", Div (V "x", V "y"))), Num 8)
let lam7 = App (Lam ("x", Lam ("y", Mod (V "x", V "y"))), Num 10)
let lam8 = App (Lam ("x", Lam ("y", Exp (V "x", V "y"))), Num 2)
let lam9 = App (Lam ("x", Lam ("y", Eq (V "x", V "y"))), Num 5)
let lam10 = App (Lam ("x", Lam ("y", Neq (V "x", V "y"))), Num 5)
let lam11 = App (Lam ("x", Lam ("y", Gt (V "x", V "y"))), Num 5)
let lam12 = App (Lam ("x", Lam ("y", Ge (V "x", V "y"))), Num 5)
let lam17 = Lam ("x", Not (V "x"))
let lam18 = Lam ("x", Absolute (V "x"))
let lam19 = Lam ("x", IfThenElse (V "x", Num 1, Num 0))
let lam20 = Lam ("x", App (Lam ("y", Add (V "x", V "y")), Num 42))
let lam21 = App (Lam ("x", Lam ("y", Mul (V "x", V "y"))), Num 3)
let lam22 = App (Lam ("x", App (Lam ("y", Sub (V "x", V "y")), Num 10)), Num 5)
let lam23 = App (Lam ("x", App (Lam ("y", Div (V "x", V "y")), Num 2)), Num 8)
let lam24 = App (Lam ("x", App (Lam ("y", Mod (V "x", V "y")), Num 3)), Num 10)
let lam25 = App (Lam ("x", App (Lam ("y", Exp (V "x", V "y")), Num 2)), Num 3)

let lam26 =
  App
    ( Lam
        ( "x",
          App (Lam ("y", IfThenElse (Lt (V "x", V "y"), Num 1, Num 0)), Num 5)
        ),
      Num 3 )

let test_lam1 = lamexp_to_string (unload lam1 []) = "15"
let test_lam2 = lamexp_to_string (unload lam2 []) = "20"
let test_lam3 = lamexp_to_string (unload (App (lam3, Num 5)) []) = "10"
let test_lam4 = lamexp_to_string (unload (App (lam4, Num 10)) []) = "0"
let test_lam5 = lamexp_to_string (unload (App (lam5, Num 4)) []) = "16"
let test_lam6 = lamexp_to_string (unload (App (lam6, Num 8)) []) = "1"
let test_lam7 = lamexp_to_string (unload (App (lam7, Num 3)) []) = "1"
let test_lam8 = lamexp_to_string (unload (App (lam8, Num 5)) []) = "32"
let test_lam9 = lamexp_to_string (unload (App (lam9, Num 5)) []) = "true"
let test_lam10 = lamexp_to_string (unload (App (lam10, Num 5)) []) = "false"
let test_lam11 = lamexp_to_string (unload (App (lam11, Num 5)) []) = "false"
let test_lam12 = lamexp_to_string (unload (App (lam12, Num 5)) []) = "true"
let test_lam13 = lamexp_to_string (unload (App (lam17, B true)) []) = "false"
let test_lam14 = lamexp_to_string (unload (App (lam18, Num (-5))) []) = "5"
let test_lam15 = lamexp_to_string (unload (App (lam19, B true)) []) = "1"
let test_lam16 = lamexp_to_string (unload (App (lam20, Num 42)) []) = "84"
let test_lam17 = lamexp_to_string (unload (App (lam21, Num 4)) []) = "12"
let test_lam18 = lamexp_to_string (unload lam22 []) = "-5"
let test_lam19 = lamexp_to_string (unload lam23 []) = "4"
let test_lam20 = lamexp_to_string (unload lam24 []) = "1"
let test_lam21 = lamexp_to_string (unload lam25 []) = "9"
let test_lam22 = lamexp_to_string (unload lam26 []) = "1"

let test () =
  let tests =
    [
      test_lam1;
      test_lam2;
      test_lam3;
      test_lam4;
      test_lam5;
      test_lam6;
      test_lam7;
      test_lam8;
      test_lam9;
      test_lam10;
      test_lam11;
      test_lam12;
      test_lam13;
      test_lam14;
      test_lam15;
      test_lam16;
      test_lam17;
      test_lam18;
      test_lam19;
      test_lam20;
      test_lam21;
      test_lam22;
    ]
  in
  List.iteri
    (fun i test ->
      if test then Printf.printf "Test %d passed\n" (i + 1)
      else Printf.printf "Test %d failed\n" (i + 1))
    tests

let test_call_by_name () =
  (* Test 1: Call-by-name evaluation with a non-terminating expression *)
  let lam1 = App (Lam ("x", Num 42), App (Lam ("y", V "y"), V "y")) in
  let result1 = unload lam1 [] in
  assert (result1 = Num 42);

  (* Test 2: Call-by-name evaluation with a conditional expression *)
  let lam2 =
    App
      ( Lam
          ( "x",
            IfThenElse
              ( Eq (V "x", Num 0),
                Num 1,
                App (Lam ("y", Div (Num 1, V "y")), Num 0) ) ),
        Num 0 )
  in
  let result2 = unload lam2 [] in
  assert (result2 = Num 1);

  (* Test 3: Call-by-name evaluation with a lambda that is never used *)
  let lam3 = App (Lam ("x", Num 10), Lam ("y", App (V "y", Num 5))) in
  let result3 = unload lam3 [] in
  assert (result3 = Num 10);

  (* Test 4: Call-by-name evaluation with a nested application *)
  let lam4 =
    App
      (Lam ("x", Add (V "x", Num 5)), App (Lam ("y", Mul (Num 2, V "y")), Num 3))
  in
  let result4 = unload lam4 [] in
  assert (result4 = Num 11);

  (* Test 5: Call-by-name evaluation with a non-terminating branch *)
  let lam5 =
    App
      ( Lam
          ( "x",
            IfThenElse
              ( Eq (V "x", Num 0),
                Num 1,
                App (Lam ("y", App (V "y", V "y")), Lam ("z", V "z")) ) ),
        Num 0 )
  in
  let result5 = unload lam5 [] in
  assert (result5 = Num 1);

  Printf.printf "All call-by-name tests passed!\n"

let () = test_call_by_name ()
