type exp =
  | Num of int
  | B of bool
  | Add of exp * exp
  | Div of exp * exp
  | Var of string
  | Abs of string * exp
  | App of exp * exp
  | IfThenElse of exp * exp * exp

type value = NumV of int | BoolV of bool | FunV of string * exp

type clos = Clos of exp * table | Vclos of value * table
and table = (string * clos) list

type stack_token =
  | APPTOK of clos
  | ADDTOK of clos
  | IFTETOK of clos * clos
  | DIVTOK of clos

let rec aug t s x =
  match t with
  | [] -> [ (s, x) ]
  | (s1, x) :: t1 -> if s = s1 then (s, x) :: t1 else (s1, x) :: aug t1 s x

let rec lookup s t =
  match t with
  | [] -> failwith ("unbound variable " ^ s)
  | (s1, x) :: t1 -> if s = s1 then x else lookup s t1

let rec combine t1 t2 =
  match t1 with [] -> t2 | (s, x) :: t1' -> combine t1' (aug t2 s x)

let rec krivine_call_by_name e s =
  match e with
  | Vclos (NumV n, t) -> (
      match s with
      | [] -> NumV n
      | ADDTOK (Vclos (NumV n1, _)) :: s1 ->
          krivine_call_by_name (Vclos (NumV (n + n1), t)) s1
      | DIVTOK (Vclos (NumV n1, _)) :: s1 ->
          if n1 = 0 then failwith "Division by zero"
          else krivine_call_by_name (Vclos (NumV (n / n1), t)) s1
      | ADDTOK (Clos (x1, t1)) :: s1 ->
          krivine_call_by_name (Clos (x1, t1)) (ADDTOK (Vclos (NumV n, t)) :: s1)
      | DIVTOK (Clos (x1, t1)) :: s1 ->
          krivine_call_by_name (Clos (x1, t1)) (DIVTOK (Vclos (NumV n, t)) :: s1)
      | _ -> failwith "Invalid stack state1")
  | Vclos (BoolV b, _t) -> (
      match s with
      | [] -> BoolV b
      | IFTETOK (c1, c2) :: s1 ->
          if b then krivine_call_by_name c1 s1 else krivine_call_by_name c2 s1
      | _ -> failwith "Invalid stack state2")
  | Vclos (FunV (x, e1), t) -> (
      match s with
      | [] -> FunV (x, e1)
      | APPTOK c :: s1 -> krivine_call_by_name (Clos (e1, aug t x c)) s1
      | _ -> failwith "Invalid stack state3")
  | Clos (Var x, t) -> krivine_call_by_name (lookup x t) s
  | Clos (Num n, t) -> krivine_call_by_name (Vclos (NumV n, t)) s
  | Clos (B b, t) -> krivine_call_by_name (Vclos (BoolV b, t)) s
  | Clos (Add (e1, e2), t) ->
      krivine_call_by_name (Clos (e2, t)) (ADDTOK (Clos (e1, t)) :: s)
  | Clos (Div (e1, e2), t) ->
      krivine_call_by_name (Clos (e2, t)) (DIVTOK (Clos (e1, t)) :: s)
  | Clos (Abs (x, e1), t) -> krivine_call_by_name (Vclos (FunV (x, e1), t)) s
  | Clos (App (e1, e2), t) ->
      krivine_call_by_name (Clos (e1, t)) (APPTOK (Clos (e2, t)) :: s)
  | Clos (IfThenElse (e1, e2, e3), t) ->
      krivine_call_by_name
        (Clos (e1, t))
        (IFTETOK (Clos (e2, t), Clos (e3, t)) :: s)

let sample_table =
  [
    ("x", Clos (Num 5, []));
    ("y", Clos (Div (Num 5, Num 0), []));
    ("z", Vclos (BoolV true, []));
  ]

let testcode e =
  let ans = krivine_call_by_name e [] in
  match ans with
  | NumV n -> Printf.printf "Result: %d\n" n
  | _ -> failwith "Invalid result type"

let test () =
  let e1 = Clos (Add (Num 1, Num 2), []) in
  let e2 = Clos (App (Abs ("x", Add (Var "x", Num 1)), Num 6), []) in
  let e3 =
    Clos
      ( App
          ( Abs ("x", Add (Var "x", Num 1)),
            App (Abs ("y", Add (Var "y", Num 2)), Num 6) ),
        [] )
  in
  let e4 =
    Clos
      ( App
          ( Abs ("x", Add (Var "x", Num 1)),
            App
              ( Abs ("y", Add (Var "y", Num 2)),
                App (Abs ("z", Add (Var "z", Num 3)), Num 6) ) ),
        [] )
  in
  let e5 =
    Clos
      ( IfThenElse
          ( B true,
            App (Abs ("x", Add (Var "x", Num 1)), Num 6),
            App (Abs ("x", Div (Var "x", Num 0)), Num 2) ),
        [] )
  in
  let e6 = Clos (Add (Num 1, Var "x"), sample_table) in
  testcode e1;
  testcode e2;
  testcode e3;
  testcode e4;
  testcode e5;
  testcode e6
