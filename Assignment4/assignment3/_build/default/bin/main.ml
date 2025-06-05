(* ---------- TEST FRAMEWORK ---------- *)

let test_case name f =
  try
    f ();
    Printf.printf "Test '%s' passed!\n" name
  with
  | Assert_failure _ ->
      Printf.printf "Test '%s' FAILED (assertion failure)!\n" name
  | e ->
      Printf.printf "Test '%s' FAILED with exception: %s\n" name (Printexc.to_string e)

(* ---------- KRIVINE MACHINE TESTS ---------- *)

open Assignment3.Krivine_cbn  (* Assuming your krivine.ml exposes Krivine module *)

let test_krivine_var_lookup () =
  let env = [ (V "x", Clos (V "x", []))] in
  let cl = V "x" in
  (* let result = krivine_machine cl [] in *)
  (* Printf.printf "%s\n" (lamexp_to_string (unload cl env)); *)
  assert ((unload cl env) = UBV "x")

let test_krivine_identity_function () =
  let id = Lam ("x", V "x") in
  let env = [ (V "y", Clos (V "y", []))] in
  let cl = App (id, V "y") in
  (* let result = krivine_exec cl [] in *)
  assert (unload cl env = UBV "y")

let test_krivine_nested_lambda () =
  let term = App (Lam ("x", Lam ("y", V "x")), V "z") in
  let env = [ (V "z", Clos (V "z", []))] in
  let _cl = Clos (term, env) in
  (* let result = krivine_exec cl [] in *)
  match unload term env with
  | Lam ("y", UBV "z") -> ()
  | _ -> assert false

let test_krivine_application_of_functions () =
  let f = Lam ("x", App (V "x", V "x")) in
  let arg = Lam ("y", V "y") in
  let env = [] in
  let term = App (f, arg) in
  (* let cl = Clos (term, env) in *)
  (* let result = krivine_exec cl [] in *)
  match unload term env with
  | Lam ("y", UBV "y") -> ()
  | _ -> assert false

let test_krivine_free_variable () =
  let term = Lam ("x", V "y") in
  let env = [ (V "z", Clos (V "z", []))] in
  let cl = App (term, V "z") in
  (* let result = krivine_exec cl [] in *)
  (* Printf.printf "%s\n" (lamexp_to_string (unload term env));  *)
  match unload cl env with
  | UBV "y" -> ()
  | _ -> assert false

(* ---------- SECD MACHINE TESTS ---------- *)

open Assignment3.Secd  (* Assuming your secd.ml exposes SECD module *)

let test_secd_var_lookup () =
  let env = [ ("x", VClos ([], "x", []))] in
  let s, e, c, d = ([], env, [LOOKUP "x"], []) in
  let result = secd_machine s e c d in
  match result with
  | VClos (_, "x", [])  -> ()  (* Checking for the closure *)
  | _ -> assert false

let test_secd_simple_function_application () =
  let lam = MKCLOS ("x", [LOOKUP "x"; RET]) in
  let prog = [lam; lam; APP] in
  let result = secd_machine [] [] prog [] in
  match result with
  | VClos (_, "x", [LOOKUP "x"; RET]) -> ()  (* Checking for the closure *)
  | _ -> assert false

let test_secd_nested_functions () =
  let inner = MKCLOS ("y", [LOOKUP "y"; RET]) in
  let outer = MKCLOS ("x", [inner; RET]) in
  let prog = [outer; outer; APP] in
  let result = secd_machine [] [] prog [] in
  match result with
  | VClos (_, "y", [LOOKUP "y"; RET]) -> ()  (* Checking for the closure *)
  | _ -> assert false

let test_secd_application_ret () =
  let lam = MKCLOS ("x", [LOOKUP "x"; RET]) in
  let prog = [lam; lam; APP] in
  let result = secd_machine [] [] prog [] in
  match result with
  | VClos (_, "x", [LOOKUP "x"; RET]) -> ()  (* Checking for the closure *)
  | _ -> assert false

  let test_secd_closure_environment () =
    let lam = MKCLOS ("x", [LOOKUP "x"; RET]) in
    let env = [ ("y", VClos ([], "y", []))] in
    let s, e, c, d = ([], env, [lam], []) in
    let result = secd_machine s e c d in
    match result with
    | VClos (closure_env, "x", [LOOKUP "x"; RET]) ->
        (* Check that closure_env contains an  for "y" *)
        let rec contains_y = function
          |  ("y", _) :: _ -> true
          | _ :: rest -> contains_y rest
          | [] -> false
        in
        assert (contains_y closure_env)  (* Check for closure environment *)
    | _ -> assert false
  
(* ---------- RUN ALL TESTS ---------- *)
 

let () =
  (* Krivine Tests *)
  test_case "Krivine Var Lookup" test_krivine_var_lookup;
  test_case "Krivine Identity Function" test_krivine_identity_function;
  test_case "Krivine Nested Lambda" test_krivine_nested_lambda;
  test_case "Krivine Application of Functions" test_krivine_application_of_functions;
  test_case "Krivine Free Variable" test_krivine_free_variable;

  (* SECD Tests *)
  test_case "SECD Var Lookup" test_secd_var_lookup;
  test_case "SECD Simple Function Application" test_secd_simple_function_application;
  test_case "SECD Nested Functions" test_secd_nested_functions;
  test_case "SECD Application RET" test_secd_application_ret;
  test_case "SECD Closure Environment" test_secd_closure_environment 
