type variable = string
type symbol = variable*int
type term = V of variable | Node of symbol * (term array)

type signature = symbol list

type substitution = (variable * term) list

exception NOT_UNIFIABLE
exception INVALID_POSITION
let rec check_arity s = match s with
  | [] -> true
  | (_, ar)::t -> if ar < 0 then false else check_arity t

let rec check_duplicate s = match s with
  | [] -> true
  | (x, _ar)::t -> if List.exists (fun (y, _) -> x = y) t then false else check_duplicate t

let check_signature s = check_arity s && check_duplicate s

let arrity (sym:variable) (signature:signature) = 
  let rec aux s = match s with
    | [] -> failwith "Symbol not found in signature"
    | (x, ar)::t -> if x = sym then ar else aux t
  in aux signature

let wfterm t (s:signature) = 
  if check_signature s then
    let rec help t s = match t with
    | V _v -> true
    | Node ((x,_y), ts) -> 
        let ar = arrity x s in
        if Array.length ts <> ar then false else 
          let rec aux i = match i with
            | i when i >= ar -> true
            | _ -> if (help ts.(i) s) then aux (i + 1) else false
          in aux 0
        in
        help t s

  else
    failwith "Signature is not well-formed"

let rec ht t = match t with
  | V _v -> 0
  | Node (_, ts) -> 
      let ht_list = Array.to_list (Array.map ht ts) in
      List.fold_left max 0 ht_list + 1

let rec size t = match t with
  | V _v -> 1
  | Node (_, ts) -> 
      let size_list = Array.to_list (Array.map size ts) in
      List.fold_left (+) 0 size_list + 1

let rec vars t = match t with
  | V v -> [v]
  | Node (_, ts) -> 
      let vars_list = Array.to_list (Array.map vars ts) in
      List.flatten vars_list
      |> List.sort_uniq String.compare

let rec search sigma v = match sigma with
    | [] -> V v
    | (x, t)::t' -> if x = v then t else search t' v

let rec subst sigma t = match t with
    | V v -> search sigma v
    | Node (x, [||]) -> Node (x, [||])
    | Node (x, ts) -> 
        let ts' = Array.map (subst sigma) ts in
        Node (x, ts')

let rec occurs v t = match t with
| V v' -> v = v'
| Node (_, ts) -> 
    let ts_list = Array.to_list (Array.map (occurs v) ts) in
    List.exists (fun x -> x) ts_list

let rec avail sigma v = match (sigma, v) with
| [], _-> false
| (x, _y)::sigma', (p1, _) -> if x = p1 then true else avail sigma' v
let comp sigma1 sigma2 = 
  let rec aux1 s1 s2 =
    match (s1, s2) with
    | [], [] -> []
    | s1, [] -> s1
    | [], s2 -> s2
    | (v1, t1)::s1, s2 -> (v1, subst s2 t1)::(aux1 s1 s2) in
  let rec aux2 s1 s2 =
    match (s1, s2) with 
    | (_s1, []) -> []
    | (s1, x::s2) -> if (avail s1 x) then aux2 s1 s2 else x::(aux2 s1 s2) in
  (aux2 (aux1 sigma1 sigma2) sigma2) @ (aux1 sigma1 sigma2)
  

let rec mgu t1 t2 = match (t1, t2) with
  | (V v1, V v2) -> if v1 = v2 then [] else [(v1, V v2)]
  | (V x, t) -> if (occurs x t) then raise NOT_UNIFIABLE else [(x, t)]
  | (t, V x) -> if (occurs x t) then raise NOT_UNIFIABLE else [(x, t)]
  | (Node (x, [||])), (Node (y, [||])) -> if x <> y then raise NOT_UNIFIABLE else []
  | (Node (x, ts1), Node (y, ts2)) -> 
      if x <> y then raise NOT_UNIFIABLE
      else if Array.length ts1 <> Array.length ts2 then raise NOT_UNIFIABLE
      else
        let n = Array.length ts1 in
        let rec unify_terms i sigma =
          if i >= n then sigma
          else
            let t1' = subst sigma ts1.(i) in
            let t2' = subst sigma ts2.(i) in
            let sigma_i = mgu t1' t2' in
            unify_terms (i+1) (comp sigma sigma_i)
        in
        unify_terms 0 []

let rec valid_position t pos =
  match (t, pos) with
  | (_, []) -> true
  | (V _, _::_) -> false
  | (Node (_, ts), i::rest) ->
      if i >= 0 && i < Array.length ts then
        valid_position ts.(i) rest
      else
        false
let rec subterm_at t pos =
  match (t, pos) with
  | (t, []) -> t
  | (V _, _::_) -> raise INVALID_POSITION
  | (Node (_, ts), i::rest) ->
      if i >= 0 && i < Array.length ts then
        subterm_at ts.(i) rest
      else
        raise INVALID_POSITION
let rec edit t pos replacement =
  if not (valid_position t pos) then
    raise INVALID_POSITION
  else
    match (t, pos) with
    | (_, []) -> replacement
    | (Node (sym, ts), i::rest) ->
        let ts' = Array.copy ts in
        ts'.(i) <- edit ts.(i) rest replacement;
        Node (sym, ts')
    | (V _, _::_) -> failwith "Cannot edit subterm of a variable"

let inplace_subst t sigma =
  let rec replace_vars term =
    match term with
    | V v ->
        (match List.assoc_opt v sigma with
        | Some replacement -> 
            replacement (* This value will be assigned by the caller *)
        | None -> term)
    | Node (_sym, ts) ->
        for i = 0 to Array.length ts - 1 do
          ts.(i) <- replace_vars ts.(i)
        done;
        term
  in
  ignore (replace_vars t) (* Perform the substitution but don't return anything *)



let sub_to_string (sub: substitution) =
  let rec aux acc = function
    | [] -> acc
    | (v, t)::rest -> aux (acc ^ v ^ " -> " ^ (match t with V v' -> v' | Node (s, _) -> fst s)) rest
  in aux "" sub
let term_to_string (t: term) =
  let rec aux t = match t with
    | V v -> v
    | Node (s, ts) ->
        let ts_str = Array.to_list (Array.map aux ts) in
        "(" ^ (fst s) ^ " " ^ (String.concat " " ts_str) ^ ")"
  in aux t
let signature_to_string (s: signature) =
  match s with
  | [] -> "{}"
  | _ ->
      let element_to_string (sym_name, ar) = 
        Printf.sprintf "%s/%d" sym_name ar 
      in
      let elements = List.map element_to_string s in
      "{" ^ String.concat ", " elements ^ "}"

let t1 = Node (("f", 2), [|V "x"; V "x"|])
let t2 = Node (("f", 2), [|V "y"; V "z"|])
let sig0 = []


let mgu1 = mgu t1 t2
let check () =
  Printf.printf "Substitution: %s\n" (sub_to_string mgu1);
  if check_signature sig0 then Printf.printf "correct" else Printf.printf "wrong"

(* =================== TESTS =================== *)

(* let test_check_arity () =
  let test1 = check_arity [("f", 2); ("g", 1); ("h", 0)] in
  let test2 = check_arity [("f", -1); ("g", 1)] in
  Printf.printf "check_arity tests:\n";
  Printf.printf "  Valid signature: %b (Expected: true)\n" test1;
  Printf.printf "  Invalid signature (negative arity): %b (Expected: false)\n" test2;
  test1 && not test2

let test_check_duplicate () =
  let test1 = check_duplicate [("f", 2); ("g", 1); ("h", 0)] in
  let test2 = check_duplicate [("f", 2); ("f", 1)] in
  Printf.printf "\ncheck_duplicate tests:\n";
  Printf.printf "  No duplicates: %b (Expected: true)\n" test1;
  Printf.printf "  With duplicates: %b (Expected: false)\n" test2;
  test1 && not test2

let test_check_signature () =
  let test1 = check_signature [("f", 2); ("g", 1); ("h", 0)] in
  let test2 = check_signature [("f", -1); ("g", 1)] in
  let test3 = check_signature [("f", 2); ("f", 1)] in
  Printf.printf "\ncheck_signature tests:\n";
  Printf.printf "  Valid signature: %b (Expected: true)\n" test1;
  Printf.printf "  Invalid arity: %b (Expected: false)\n" test2;
  Printf.printf "  Duplicate symbols: %b (Expected: false)\n" test3;
  test1 && not test2 && not test3

let test_arrity () =
  let sig1 = [("f", 2); ("g", 1); ("h", 0)] in
  Printf.printf "\narrity tests:\n";
  try
    let ar1 = arrity "f" sig1 in
    let ar2 = arrity "g" sig1 in
    let ar3 = arrity "h" sig1 in
    Printf.printf "  Arity of f: %d (Expected: 2)\n" ar1;
    Printf.printf "  Arity of g: %d (Expected: 1)\n" ar2;
    Printf.printf "  Arity of h: %d (Expected: 0)\n" ar3;
    try
      let _ = arrity "x" sig1 in
      Printf.printf "  Arity of x: Failed to raise exception\n";
      false
    with Failure _ ->
      Printf.printf "  Arity of x: Correctly raised exception\n";
      ar1 = 2 && ar2 = 1 && ar3 = 0
  with Failure _ ->
    Printf.printf "  Unexpected exception raised\n";
    false

let test_wfterm () =
  let (sig1:signature) = [("f", 2); ("g", 1); ("h", 0)] in
  let t1 = Node(("f", 2), [|V "x"; V "y"|]) in
  let t2 = Node(("g", 1), [|V "z"|]) in
  let t3 = Node(("h", 0), [||]) in
  let t4 = Node(("f", 2), [|V "x"; Node(("g", 1), [|V "z"|])|]) in
  let t5 = Node(("f", 2), [|V "x"|]) in  (* Wrong arity *)
  let t6 = Node(("j", 1), [|V "x"|]) in  (* Symbol not in signature *)
  Printf.printf "\nwfterm tests:\n";
  let test1 = wfterm t1 sig1 in
  let test2 = wfterm t2 sig1 in
  let test3 = wfterm t3 sig1 in
  let test4 = wfterm t4 sig1 in
  Printf.printf "  Simple term with correct arity: %b (Expected: true)\n" test1;
  Printf.printf "  Term with arity 1: %b (Expected: true)\n" test2;
  Printf.printf "  Term with arity 0: %b (Expected: true)\n" test3;
  Printf.printf "  Nested term: %b (Expected: true)\n" test4;
  try
    let test5 = wfterm t5 sig1 in
    Printf.printf "  Wrong arity: %b (Expected: false)\n" test5;
    try
      let _ = wfterm t6 sig1 in
      Printf.printf "  Symbol not in signature: Failed to raise exception\n";
      test1 && test2 && test3 && test4 && not test5
    with Failure _ ->
      Printf.printf "  Symbol not in signature: Correctly raised exception\n";
      test1 && test2 && test3 && test4 && not test5
  with e ->
    Printf.printf "  Unexpected exception: %s\n" (Printexc.to_string e);
    false

let test_ht () =
  let t1 = V "x" in
  let t2 = Node(("f", 2), [|V "x"; V "y"|]) in
  let t3 = Node(("f", 2), [|V "x"; Node(("g", 1), [|V "z"|])|]) in
  let t4 = Node(("f", 2), [|Node(("g", 1), [|V "z"|]); Node(("h", 0), [||])|]) in
  Printf.printf "\nht tests:\n";
  let h1 = ht t1 in
  let h2 = ht t2 in
  let h3 = ht t3 in
  let h4 = ht t4 in
  Printf.printf "  Height of variable: %d (Expected: 0)\n" h1;
  Printf.printf "  Height of simple term: %d (Expected: 1)\n" h2;
  Printf.printf "  Height of term with nested subterm: %d (Expected: 2)\n" h3;
  Printf.printf "  Height of term with two nested subterms: %d (Expected: 2)\n" h4;
  h1 = 0 && h2 = 1 && h3 = 2 && h4 = 2

let test_size () =
  let t1 = V "x" in
  let t2 = Node(("f", 2), [|V "x"; V "y"|]) in
  let t3 = Node(("f", 2), [|V "x"; Node(("g", 1), [|V "z"|])|]) in
  Printf.printf "\nsize tests:\n";
  let s1 = size t1 in
  let s2 = size t2 in
  let s3 = size t3 in
  Printf.printf "  Size of variable: %d (Expected: 1)\n" s1;
  Printf.printf "  Size of simple term: %d (Expected: 3)\n" s2;
  Printf.printf "  Size of term with nested subterm: %d (Expected: 4)\n" s3;
  s1 = 1 && s2 = 3 && s3 = 4

let test_vars () =
  let t1 = V "x" in
  let t2 = Node(("f", 2), [|V "x"; V "y"|]) in
  let t3 = Node(("f", 2), [|V "x"; Node(("g", 1), [|V "z"|])|]) in
  let t4 = Node(("f", 2), [|V "x"; V "x"|]) in
  Printf.printf "\nvars tests:\n";
  let v1 = vars t1 in
  let v2 = vars t2 in
  let v3 = vars t3 in
  let v4 = vars t4 in
  Printf.printf "  Variables of a single variable: %s (Expected: [x])\n" 
    (String.concat ", " v1);
  Printf.printf "  Variables of simple term: %s (Expected: [x, y])\n" 
    (String.concat ", " v2);
  Printf.printf "  Variables of nested term: %s (Expected: [x, z])\n" 
    (String.concat ", " v3);
  Printf.printf "  Variables with duplicates: %s (Expected: [x])\n" 
    (String.concat ", " v4);
  v1 = ["x"] && v2 = ["x"; "y"] && v3 = ["x"; "z"] && v4 = ["x"]

let test_search () =
  let sigma = [("x", Node(("f", 1), [|V "y"|])); ("z", V "w")] in
  Printf.printf "\nsearch tests:\n";
  let s1 = search sigma "x" in
  let s2 = search sigma "z" in
  let s3 = search sigma "y" in
  Printf.printf "  Search for x: %s (Expected: (f y))\n" (term_to_string s1);
  Printf.printf "  Search for z: %s (Expected: w)\n" (term_to_string s2);
  Printf.printf "  Search for y (not in subst): %s (Expected: y)\n" (term_to_string s3);
  match (s1, s2, s3) with
  | (Node(("f", _), [|V "y"|]), V "w", V "y") -> true
  | _ -> false

let test_subst () =
  let sigma = [("x", Node(("f", 1), [|V "y"|])); ("z", V "w")] in
  let t1 = V "x" in
  let t2 = V "a" in
  let t3 = Node(("g", 2), [|V "x"; V "z"|]) in
  Printf.printf "\nsubst tests:\n";
  let s1 = subst sigma t1 in
  let s2 = subst sigma t2 in
  let s3 = subst sigma t3 in
  Printf.printf "  Substitute in variable x: %s (Expected: (f y))\n" (term_to_string s1);
  Printf.printf "  Substitute in variable not in subst: %s (Expected: a)\n" (term_to_string s2);
  Printf.printf "  Substitute in compound term: %s (Expected: (g (f y) w))\n" (term_to_string s3);
  match (s1, s2, s3) with
  | (Node(("f", _), [|V "y"|]), V "a", Node(("g", _), [|Node(("f", _), [|V "y"|]); V "w"|])) -> true
  | _ -> false

let test_occurs () =
  let t1 = V "x" in
  let t2 = Node(("f", 2), [|V "y"; V "z"|]) in
  let t3 = Node(("f", 2), [|V "y"; Node(("g", 1), [|V "x"|])|]) in
  Printf.printf "\noccurs tests:\n";
  let o1 = occurs "x" t1 in
  let o2 = occurs "x" t2 in
  let o3 = occurs "x" t3 in
  Printf.printf "  'x' occurs in V \"x\": %b (Expected: true)\n" o1;
  Printf.printf "  'x' occurs in term without x: %b (Expected: false)\n" o2;
  Printf.printf "  'x' occurs in nested term: %b (Expected: true)\n" o3;
  o1 && not o2 && o3

let test_avail () =
  let sigma = [("x", V "y"); ("z", V "w")] in
  Printf.printf "\navail tests:\n";
  let a1 = avail sigma ("x", 0) in
  let a2 = avail sigma ("y", 0) in
  let a3 = avail sigma ("z", 0) in
  Printf.printf "  'x' available in sigma: %b (Expected: true)\n" a1;
  Printf.printf "  'y' available in sigma: %b (Expected: false)\n" a2;
  Printf.printf "  'z' available in sigma: %b (Expected: true)\n" a3;
  a1 && not a2 && a3

let test_comp () =
  let sigma1 = [("x", V "y"); ("z", Node(("f", 1), [|V "w"|]))] in
  let sigma2 = [("y", V "a"); ("w", V "b")] in
  Printf.printf "\ncomp tests:\n";
  let c = comp sigma1 sigma2 in
  Printf.printf "  Composition result: %s\n" (sub_to_string c);
  Printf.printf "  Expected contains: x -> a, z -> (f b), y -> a, w -> b\n";
  List.exists (fun (v, t) -> v = "x" && t = V "a") c &&
  List.exists (fun (v, t) -> v = "z" && match t with Node(("f", _), [|V "b"|]) -> true | _ -> false) c

let test_mgu () =
  let t1 = Node(("f", 2), [|V "x"; V "y"|]) in
  let t2 = Node(("f", 2), [|V "z"; V "z"|]) in
  let t3 = Node(("g", 1), [|V "a"|]) in
  let t4 = Node(("f", 2), [|V "x"; V "x"|]) in
  let t5 = Node(("f", 2), [|V "y"; Node(("g", 1), [|V "y"|])|]) in
  Printf.printf "\nmgu tests:\n";
  try
    let m1 = mgu t1 t2 in
    Printf.printf "  MGU of (f x y) and (f z z): %s\n" (sub_to_string m1);
    try
      let _m2 = mgu t1 t3 in
      Printf.printf "  MGU of different function symbols: Failed to raise NOT_UNIFIABLE\n";
      false
    with NOT_UNIFIABLE ->
      Printf.printf "  MGU of different function symbols: Correctly raised NOT_UNIFIABLE\n";
      try
        let _m3 = mgu t4 t5 in
        Printf.printf "  MGU with occurs check: Failed to raise NOT_UNIFIABLE\n";
        false
      with NOT_UNIFIABLE ->
        Printf.printf "  MGU with occurs check: Correctly raised NOT_UNIFIABLE\n";
        true
  with e ->
    Printf.printf "  Unexpected exception: %s\n" (Printexc.to_string e);
    false

let test_valid_position () =
  let t = Node(("f", 2), [|V "x"; Node(("g", 1), [|V "y"|])|]) in
  Printf.printf "\nvalid_position tests:\n";
  let vp1 = valid_position t [] in
  let vp2 = valid_position t [0] in
  let vp3 = valid_position t [1] in
  let vp4 = valid_position t [1; 0] in
  let vp5 = valid_position t [2] in
  let vp6 = valid_position t [0; 0] in
  Printf.printf "  Root position: %b (Expected: true)\n" vp1;
  Printf.printf "  First argument: %b (Expected: true)\n" vp2;
  Printf.printf "  Second argument: %b (Expected: true)\n" vp3;
  Printf.printf "  Subterm in second argument: %b (Expected: true)\n" vp4;
  Printf.printf "  Invalid index: %b (Expected: false)\n" vp5;
  Printf.printf "  Invalid subterm of variable: %b (Expected: false)\n" vp6;
  vp1 && vp2 && vp3 && vp4 && not vp5 && not vp6

let test_subterm_at () =
  let t = Node(("f", 2), [|V "x"; Node(("g", 1), [|V "y"|])|]) in
  Printf.printf "\nsubterm_at tests:\n";
  try
    let st1 = subterm_at t [] in
    let st2 = subterm_at t [0] in
    let st3 = subterm_at t [1] in
    let st4 = subterm_at t [1; 0] in
    Printf.printf "  Root position: %s (Expected: (f x (g y)))\n" (term_to_string st1);
    Printf.printf "  First argument: %s (Expected: x)\n" (term_to_string st2);
    Printf.printf "  Second argument: %s (Expected: (g y))\n" (term_to_string st3);
    Printf.printf "  Subterm in second argument: %s (Expected: y)\n" (term_to_string st4);
    try
      let _ = subterm_at t [2] in
      Printf.printf "  Invalid index: Failed to raise INVALID_POSITION\n";
      false
    with INVALID_POSITION ->
      Printf.printf "  Invalid index: Correctly raised INVALID_POSITION\n";
      try
        let _ = subterm_at t [0; 0] in
        Printf.printf "  Invalid subterm of variable: Failed to raise INVALID_POSITION\n";
        false
      with INVALID_POSITION ->
        Printf.printf "  Invalid subterm of variable: Correctly raised INVALID_POSITION\n";
        true
  with e ->
    Printf.printf "  Unexpected exception: %s\n" (Printexc.to_string e);
    false

let test_edit () =
  let t = Node(("f", 2), [|V "x"; Node(("g", 1), [|V "y"|])|]) in
  let replacement = V "z" in
  Printf.printf "\nedit tests:\n";
  try
    let e1 = edit t [0] replacement in
    let e2 = edit t [1; 0] replacement in
    Printf.printf "  Edit first argument: %s (Expected: (f z (g y)))\n" (term_to_string e1);
    Printf.printf "  Edit subterm in second argument: %s (Expected: (f x (g z)))\n" (term_to_string e2);
    try
      let _ = edit t [2] replacement in
      Printf.printf "  Invalid position: Failed to raise INVALID_POSITION\n";
      false
    with INVALID_POSITION ->
      Printf.printf "  Invalid position: Correctly raised INVALID_POSITION\n";
      true
  with e ->
    Printf.printf "  Unexpected exception: %s\n" (Printexc.to_string e);
    false

let test_inplace_subst () =
  let t = Node(("f", 2), [|V "x"; Node(("g", 1), [|V "y"|])|]) in
  let sigma = [("x", V "z"); ("y", Node(("h", 0), [||]))] in
  Printf.printf "\ninplace_subst tests:\n";
  let original = term_to_string t in
  inplace_subst t sigma;  (* No result assignment since function doesn't return *)
  let after = term_to_string t in
  Printf.printf "  Original term: %s\n" original;
  Printf.printf "  After substitution: %s (Expected: (f z (g (h))))\n" after;
  match t with
  | Node(("f", _), [|V "z"; Node(("g", _), [|Node(("h", _), [||])|])|]) -> true
  | _ -> false

let test_string_functions () =
  let sub = [("x", V "y"); ("z", Node(("f", 1), [|V "w"|]))] in
  let term = Node(("f", 2), [|V "x"; Node(("g", 1), [|V "y"|])|]) in
  let sig1 = [("f", 2); ("g", 1); ("h", 0)] in
  Printf.printf "\nString conversion tests:\n";
  Printf.printf "  sub_to_string: %s\n" (sub_to_string sub);
  Printf.printf "  term_to_string: %s\n" (term_to_string term);
  Printf.printf "  signature_to_string: %s\n" (signature_to_string sig1);
  true

let run_all_tests () =
  Printf.printf "Running all tests...\n";
  let results = [
    test_check_arity ();
    test_check_duplicate ();
    test_check_signature ();
    test_arrity ();
    test_wfterm ();
    test_ht ();
    test_size ();
    test_vars ();
    test_search ();
    test_subst ();
    test_occurs ();
    test_avail ();
    test_comp ();
    test_mgu ();
    test_valid_position ();
    test_subterm_at ();
    test_edit ();
    test_inplace_subst ();
    test_string_functions ();
  ] in
  let passed = List.filter (fun x -> x) results in
  Printf.printf "\n\nTest Summary: %d/%d tests passed\n" 
    (List.length passed) (List.length results);
  if List.length passed = List.length results then
    Printf.printf "All tests passed!\n"
  else
    Printf.printf "Some tests failed!\n"

let () = run_all_tests ()

let edge_test1 () =
  Printf.printf "EDGE CASE TESTS\n";
  Printf.printf "--------------\n";
  
  (* Test 1: Empty terms with different symbols *)
  Printf.printf "Test 1: Empty terms with different symbols\n";
  let t1 = Node(("a", 0), [||]) in
  let t2 = Node(("b", 0), [||]) in
  try
    let _ = mgu t1 t2 in
    Printf.printf "FAIL: Should have raised NOT_UNIFIABLE\n"
  with
    | NOT_UNIFIABLE -> Printf.printf "PASS: Correctly raised NOT_UNIFIABLE\n"
    | _ -> Printf.printf "FAIL: Wrong exception raised\n";
  Printf.printf "\n";;
let edge_test2 () = 
  (* Test 2: Deep nesting terms *)
  Printf.printf "Test 2: Deep nesting terms\n";
  let deep_term = 
    Node(("f", 1), [|
      Node(("f", 1), [|
        Node(("f", 1), [|
          Node(("f", 1), [|
            Node(("f", 1), [|
              V "x"
            |])
          |])
        |])
      |])
    |]) in
  Printf.printf "Height of deep term: %d (Expected: 5)\n" (ht deep_term);
  Printf.printf "Size of deep term: %d (Expected: 6)\n" (size deep_term);
  Printf.printf "\n";;
  
 let edge_test_sp () =  (* Test 3: Occurs check with nested variables *)
  Printf.printf "Test 3: Occurs check with nested variables\n";
  let t3 = V "x" in
  let t4 = Node(("f", 1), [|V "x"|]) in
  try
    let _ = mgu t3 t4 in
    Printf.printf "FAIL: Should have raised NOT_UNIFIABLE\n"
  with
    | NOT_UNIFIABLE -> Printf.printf "Correctly raised NOT_UNIFIABLE\n"
    | _ -> Printf.printf "FAIL: Wrong exception raised\n";
  Printf.printf "\n";;


  
  (* Test 4: Complex substitution composition *)

let edge01() =  Printf.printf "Test 4: Complex substitution composition\n";
  let sigma1 = [("x", V "y"); ("y", V "z")] in
  let sigma2 = [("z", V "a"); ("w", V "b")] in
  let result = comp sigma1 sigma2 in
  Printf.printf "Result: %s (Expected: [x -> a, y -> a, z -> a, w -> b])\n" (sub_to_string result);
  Printf.printf "Expected: [x -> a, y -> a, z -> a, w -> b]\n";;
  
  (* Test 5: Edge cases in position handling *)
  let edge_test3 () = Printf.printf "Test 5: Edge cases in position handling\n";
  let t5 = Node(("f", 3), [|V "x"; V "y"; V "z"|]) in
  Printf.printf "Valid position [1]: %b (Expected: true)\n" (valid_position t5 [1]);
  Printf.printf "Valid position [3]: %b (Expected: false)\n" (valid_position t5 [3]);
  Printf.printf "Valid position [-1]: %b (Expected: false)\n" (valid_position t5 [-1]);
  Printf.printf "\n";
  
  (* Test 6: Substitution with circular references *)
  Printf.printf "Test 6: Substitution with circular references\n";
  (* This isn't exactly circular, but tests substitution handling *)
  let sigma_complex = [("x", V "y"); ("y", V "z"); ("z", V "x")] in
  let term = V "x" in
  let result = subst sigma_complex term in
  Printf.printf "Result: %s (should be z)\n" (term_to_string result);
  Printf.printf "\n";
  
  (* Test 7: Unification with empty terms *)
  Printf.printf "Test 7: Unification with empty terms\n";
  let t7a = Node(("c", 0), [||]) in
  let t7b = Node(("c", 0), [||]) in
  let result = mgu t7a t7b in
  Printf.printf "MGU result length: %d (Expected: 0, empty substitution)\n" (List.length result);
  Printf.printf "\n";
  
  (* Test 8: Edit at invalid positions *)
  Printf.printf "Test 8: Edit at invalid positions\n";
  let t8 = Node(("f", 1), [|V "x"|]) in
  try
    let _ = edit t8 [1] (V "y") in
    Printf.printf "FAIL: Should have raised INVALID_POSITION\n"
  with
    | INVALID_POSITION -> Printf.printf "PASS: Correctly raised INVALID_POSITION\n"
    | _ -> Printf.printf "FAIL: Wrong exception raised\n";
  Printf.printf "\n";;
  
  (* Test 9: Inplace substitution on complex terms *)
  let edge_test4 () = Printf.printf "Test 9: Inplace substitution on complex terms\n";
  let t9 = Node(("f", 2), [|V "x"; Node(("g", 1), [|V "x"|])|]) in
  let sigma9 = [("x", V "z")] in
  inplace_subst t9 sigma9;
  Printf.printf "Result: %s (Expected: (f z (g z)))\n" (term_to_string t9);
  Printf.printf "\n";;

let () = 
  run_all_tests();
  edge_test1();
  edge_test2();
  edge_test_sp();
  edge01();
  edge_test3();
  edge_test4();
  check(); *)