open Assignment5.Subsandunif

(* Simple test to check signature functionality *)
let test_check_signature () =
  let valid_sig = [("f", 2); ("g", 1); ("h", 0)] in
  let invalid_sig1 = [("f", -1); ("g", 1)] in (* negative arity *)
  let invalid_sig2 = [("f", 2); ("f", 1)] in (* duplicate symbol *)
  
  assert (check_signature valid_sig = true);
  assert (check_signature invalid_sig1 = false);
  assert (check_signature invalid_sig2 = false);
  print_endline "All check_signature tests passed!"

(* Run tests *)
let () = 
  test_check_signature ()
