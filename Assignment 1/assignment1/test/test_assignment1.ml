open Assignment1.Vector

(* let _str = Sexplib.Sexp.to_string_hum (sexp_of_vector (scale 2.5 (create 10000000 2.2)))

let () = Printf.printf("Finished\n") *)
let vec1 = create 10000000 2.2;;
let dimm = dim vec1;;
let _vec2 = scale 2.5 vec1;;
let () = Printf.printf("Dim: %d\n") dimm;;
let () = Printf.printf("Finished\n")