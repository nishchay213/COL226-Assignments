open Toycalculator.Toy

(* let e = Plus (Num 1, Times (Num 2, Num 3))
let () = Printf.printf "%s\n" (posttrav e)

let e2 = Plus (Plus (Times (Num 2, Num 3), Num 1), Num 4)
let () = Printf.printf "%s\n" (posttrav e2) *)
(* 
let e3 = Plus (Plus (Times (Plus (Num 2, Num 3), Num 1), Num 4), Num 5)
let () = Printf.printf "%s\n" (posttrav e3)

let exp_ec = stackmc [] (compile e3)
let x1 = try eval e3 with Match_failure _ -> raise Wrong
let x2 = try eval exp_ec with Match_failure _ -> raise Wrong
let () = if x1 = x2 then
  Printf.printf "eval e3 = eval exp_ec\n"
else
  Printf.printf "eval e3 != eval exp_ec\n" *)

let test1 = Plus (Times (Num 3, Num 4), Times (Num 5, Num 6))
let test2 = Or (Not (B1 T), And (B1 T, Or (B1 F, B1 T)))
let test3 = Gt (Times (Num 5, Num 6), Times (Num 3, Num 4))
let test4 = And (Eq (test1, Num 42), Not test3)
let g1 _ = N 42
let rho1 _ = Num 42
let v1 = eval test1 g1
let v2 = eval test2 g1
let v3 = eval test3 g1
let v4 = eval test4 g1
let c1 = compile test1
let c2 = compile test2
let c3 = compile test3
let c4 = compile test4
let w1 = stackmc [] c1 rho1
let w2 = stackmc [] c2 rho1
let w3 = stackmc [] c3 rho1
let w4 = stackmc [] c4 rho1

let () =
  if v1 = eval w1 g1 && v2 = eval w2 g1 && v3 = eval w3 g1 && v4 = eval w4 g1
  then Printf.printf "All tests passed\n"
  else Printf.printf "Some tests failed\n"
