open Sexplib.Std
type vector = float list [@@deriving sexp]

exception DimensionError

(* Though not used, this function was added for avoiding erros due to precision.*)
let truncate x decimals =
  let multiplier = 10.0 ** (float_of_int decimals) in
  (floor (x *. multiplier)) /. multiplier;;

let create (n : int) (x : float) = 
  if n < 1 then
    raise  DimensionError
  else
    let rec create_n n acc= 
      if n = 0 then acc else create_n (n-1) (x::acc)
    in
    create_n n []
  ;;

let dim (v : vector) =
   
  let rec length u acc = match u with
  | [] -> acc
  | _x::xs -> length xs (acc + 1)
  in
  let len = length v 0 in
  if len < 1 then
    raise DimensionError
  else
  len
  
  ;;

let is_zero (v : vector) = 
  if (dim v) < 1 then
    raise DimensionError
  else
  let rec check_zero v = 
    match v with
    | [] -> true
    | x::xs -> if x = 0.0 then check_zero xs else false
  in
  check_zero v;;
  

let unit (n : int) (j : int) =
  if n < 1 || j < 1 || j > n then
    raise DimensionError
  else
  let rec gen_unit n j acc =
    if n = 0 then acc else gen_unit (n-1) j ((if n = j then 1.0 else 0.0)::acc)
  in
  gen_unit n j [];;

(* Here, I have used inbuilt function List.map, I could also do tail recursion here, but occam's razor. *)
let scale (x : float) (v : vector) = 
  if (dim v) < 1 then
    raise DimensionError
  else
  List.map (fun y -> x *. y) v;;


(* In addv, I have added a helper function; to first check the dimension of arguments, if empty list was considered vector, I could have made addv itself recursive. *)
let addv (v1 : vector) (v2 : vector) = 
  if (dim v1) = (dim v2) then
    let rec aux v1 v2 =
      match v1, v2 with
      | [], [] -> []
      | x1::xs1, x2::xs2 -> (x1 +. x2)::(aux xs1 xs2) 
      | _ -> raise DimensionError
    in
    aux v1 v2
  else
    raise DimensionError;;
    

let dot_prod (v1 : vector) (v2 : vector) = 
  if (dim v1) = (dim v2) then
    let rec aux v1 v2 acc =
      match v1, v2 with
      | [], [] -> acc
      | x1::xs1, x2::xs2 -> aux xs1 xs2 (acc +. (x1 *. x2))
      | _ -> raise DimensionError
    in
    aux v1 v2 0.0
  else
    raise DimensionError

let inv (v : vector) =
  if (dim v) < 1 then
    raise DimensionError
  else 
  List.map (fun x -> -.x) v;;
 
let length (v : vector) = 
  if (dim v) < 1 then
    raise DimensionError
  else
  sqrt (dot_prod v v);;

(* Because not specified which error should be raised in case of length of any vector is zero, I am raising dimension error. Zero division error could also be raised. *)
let angle (v1 : vector) (v2 : vector) =
  let len_v1 = length v1 in
  let len_v2 = length v2 in
  if len_v1 = 0.0 || len_v2 = 0.0 then
    raise DimensionError  
  else
    acos ((dot_prod v1 v2) /. (len_v1 *. len_v2));;

(* Testcases *)


let testcase1_create = fun() -> create 3 0.0 = [0.0; 0.0; 0.0];;
let testcase2_create = fun() -> create 8 1.0 = [1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0];;
let testcase3_create = fun() -> create 5 2.0 = [2.0; 2.0; 2.0; 2.0; 2.0];;
let testcase4_create = fun() -> create 1 3.7 = [3.7];;
let testcase5_create = 
  fun() -> 
  try
    let _ = create 0 3.7 in false
  with
    |DimensionError -> true
    |_-> false
  ;;
  

let testcase6_create = fun() -> 
  try
    let _ = create (-3) (-3.7) in false
  with
    |DimensionError -> true
    |_-> false
  ;;

let testcase7_create = fun() -> create 3 (-3.7) = [-3.7; -3.7; -3.7];;
(* let testcase8_create = fun() ->
  try
    let _ = create 2.2 3.7 in false
  with 
    type_error -> true *)
    (* I  tried to add testcases in such format, but because of beauty of ocaml, it won't let me compile.*)

let testcase1_dim = fun() -> dim [0.0; 0.0; 0.0] = 3;;
let testcase2_dim = fun() -> dim [5.2; 2.4; 2.0; 1.0; 0.0; 0.0; 0.0; 0.0] = 8;;
let testcase3_dim = fun() -> dim [2.0; 2.0; 2.0; 2.0; 2.0] = 5;;
let testcase4_dim = fun() -> dim [3.7] = 1;;
let testcase5_dim = fun() -> 
  try
    let _ = dim [] in false
  with
    |DimensionError -> true
    |_-> false
;;

let testcase1_is_zero = fun() -> is_zero [0.0; 0.0; 0.0] = true;;
let testcase2_is_zero = fun() -> is_zero [0.0; 0.0; 0.0; 0.0; 0.0] = true;;
let testcase3_is_zero = fun() -> is_zero [2.1; 2.3; 2.4; 2.5; 2.6] = false;;
let testcase4_is_zero = fun() -> 
  try
    let _ = is_zero [] in false
  with
    |DimensionError -> true
    |_-> false
  ;;
let testcase5_is_zero = fun() -> is_zero [1.6; 0.0; 0.0; 0.0; 0.0] = false;;
(* let testcase6_is_zero = fun() -> is_zero [2.1; 1.3; 2; 42.2; 22.4] = false;;  this kinds also don't work, as ocaml won't allow me to pass anything withuot it's desired type in it.*) 

let testcase1_unit = fun() -> unit 3 1 = [1.0; 0.0; 0.0];;
let testcase2_unit = fun() -> unit 3 2 = [0.0; 1.0; 0.0];;
let testcase3_unit = fun() -> unit 3 3 = [0.0; 0.0; 1.0];;
let testcase4_unit = fun() -> unit 5 1 = [1.0; 0.0; 0.0; 0.0; 0.0];;
let testcase5_unit = fun() -> unit 5 5 = [0.0; 0.0; 0.0; 0.0; 1.0];;
let testcase6_unit = fun() -> 
  try 
    let _ = unit 4 5 in false
  with
    |DimensionError -> true
    |_-> false
let testcase7_unit = fun() -> 
  try 
    let _ = unit 0 0 in false
  with
    |DimensionError -> true
    |_-> false
let testcase8_unit = fun() -> 
  try 
    let _ = unit 4 0 in false
  with
    |DimensionError -> true
    |_-> false

let testcase1_scale = fun() -> scale 2.0 [1.0; 2.0; 3.0] = [2.0; 4.0; 6.0];;
let testcase2_scale = fun() -> scale 0.0 [1.0; 2.0; 3.0] = [0.0; 0.0; 0.0];;
let testcase3_scale = fun() -> scale 1.0 [1.0; 2.0; 3.0] = [1.0; 2.0; 3.0];;
let testcase4_scale = fun() -> scale 3.0 [1.0; 2.0; 3.0] = [3.0; 6.0; 9.0];;
let testcase5_scale = fun() -> 
  try
    let _ = scale 4.0 [] in false
  with
    |DimensionError -> true
    |_-> false
let testcase6_scale = fun() -> scale 2.8987844 [2.4565; 21.5565; 52.1552; 531.5533] = [2.8987844*.2.4565; 2.8987844*.21.5565; 2.8987844*.52.1552; 2.8987844*.531.5533];;    

let testcase1_addv = fun() -> addv [1.0; 2.0; 3.0] [4.0; 5.0; 6.0] = [5.0; 7.0; 9.0];;
let testcase2_addv = fun() -> 
  try
    let _ = addv [1.0; 2.0; 3.0] [4.0; 5.0; 6.0; 7.0] in false
  with
    |DimensionError -> true
    |_-> false
  ;;
let testcase3_addv = fun() -> 
  try
    let _ = addv [1.0; 2.0; 3.0] [4.0; 5.0] in false
  with
    |DimensionError -> true
    |_-> false
  ;;
let testcase4_addv = fun() -> addv [1.0; 2.0; 3.0] [4.0; 5.0; 6.0] = [5.0; 7.0; 9.0];;
let testcase5_addv = fun() -> addv [2.1; 2.3; 2.4; 2.5; 2.6] [1.0; 2.0; 3.0; 4.0; 5.0] = [3.1; 4.3; 5.4; 6.5; 7.6];;

let testcase1_dot_prod = fun() -> dot_prod [1.0; 2.0; 3.0] [4.0; 5.0; 6.0] = 32.0;;
let testcase2_dot_prod = fun() -> 
  try
    let _ = dot_prod [1.0; 2.0; 3.0; 4.0] [4.0; 5.0; 6.0] in false
  with
    |DimensionError -> true
    |_-> false
  ;;
let testcase3_dot_prod = fun() -> 
  try
    let _ = dot_prod [1.0; 2.0; 3.0; 4.0; 5.0] [4.0; 5.0; 6.0; 7.0] in false
  with
    |DimensionError -> true
    |_-> false
  ;;
let testcase4_dot_prod = fun() -> truncate (dot_prod [1.0; 2.0; 3.0] [4.0; 5.0; 6.0]) 2 = 32.0;;
let testcase5_dot_prod = fun() -> truncate (dot_prod [2.1; 2.3; 2.4; 2.5; 2.6] [1.0; 2.0; 3.0; 4.0; 5.0]) 1 = 36.9;;

let testcase1_inv = fun() -> inv [1.0; 2.0; 3.0] = [-1.0; -2.0; -3.0];;
let testcase2_inv = fun() -> inv [1.0; 2.0; 3.0; 4.0] = [-1.0; -2.0; -3.0; -4.0];;
let testcase3_inv = fun() -> inv [1.0; 2.0; 3.0; 4.0; 5.0] = [-1.0; -2.0; -3.0; -4.0; -5.0];;
let testcase4_inv = fun() -> inv [1.0; 2.0; 3.0; 4.0; 5.0; 6.0] = [-1.0; -2.0; -3.0; -4.0; -5.0; -6.0];;
let testcase5_inv = fun() -> 
  try
    let _ = inv [] in false
  with
    |DimensionError -> true
    |_-> false
  ;;

let testcase1_length = fun() -> length [1.0; 2.0; 3.0] = sqrt 14.0;;
let testcase2_length = fun() -> length [1.0; 2.0; 3.0; 4.0] = sqrt 30.0;;
let testcase3_length = fun() -> length [1.0; 2.0; 3.0; 4.0; 5.0] = sqrt 55.0;;
let testcase4_length = fun() -> length [1.0; 2.0; 3.0; 4.0; 5.0; 6.0] = sqrt 91.0;;
let testcase5_length = fun() ->
  try
    let _ = length [] in false
  with
    |DimensionError -> true
    |_-> false
  ;;

let testcase1_angle = fun() -> angle [1.0; 2.0; 3.0] [4.0; 5.0; 6.0] = acos (32.0 /. (sqrt 14.0 *. sqrt 77.0));;
let testcase2_angle = fun() -> 
  try
    let _ = angle [1.0; 2.0; 3.0] [4.0; 5.0; 6.0; 7.0] in false
  with
    |DimensionError -> true
    |_-> false
  ;;
let testcase3_angle = fun() -> 
  try
    let _ = angle [1.0; 2.0; 3.0; 4.0; 5.0] [4.0; 5.0; 6.0; 7.0] in false
  with
    |DimensionError -> true
    |_-> false
  ;;
let testcase4_angle = fun() -> angle [1.0; 2.0; 3.0; 4.0; 5.0; 6.0] [4.0; 5.0; 6.0; 7.0; 8.0; 9.0] = acos (154.0 /. (sqrt 91.0 *. sqrt 271.0));;
let testcase5_angle = fun() ->  
  try
    let _ = angle [] [] in false
  with
    |DimensionError -> true
    |_-> false
  ;;

  let test_create () = 
    Printf.printf "Testing create...\n";
    if testcase1_create () && testcase2_create() && testcase3_create() && testcase4_create() && testcase5_create() && testcase6_create() && testcase7_create() then
      print_endline "test_create passed"
    else
      print_endline "test_create failed";;
  
  let test_dim () =
    Printf.printf "Testing dim...\n";
    if testcase1_dim() && testcase2_dim() && testcase3_dim() && testcase4_dim() && testcase5_dim() then
      print_endline "test_dim passed"
    else
      print_endline "test_dim failed";;
  
  let test_is_zero () =
    Printf.printf "Testing is_zero...\n";
    if testcase1_is_zero() && testcase2_is_zero() && testcase3_is_zero() && testcase4_is_zero() && testcase5_is_zero() then
      print_endline "test_is_zero passed"
    else
      print_endline "test_is_zero failed";;
  
  let test_unit () =
    Printf.printf "Testing unit...\n";
    if testcase1_unit() && testcase2_unit() && testcase3_unit() && testcase4_unit() && testcase5_unit() && testcase6_unit() && testcase7_unit() && testcase8_unit() then
      print_endline "test_unit passed"
    else
      print_endline "test_unit failed";;
  
  let test_scale () =
    Printf.printf "Testing scale...\n";
    if testcase1_scale() && testcase2_scale() && testcase3_scale() && testcase4_scale() && testcase5_scale() && testcase6_scale() then
      print_endline "test_scale passed"
    else
      print_endline "test_scale failed";;
  
  let test_addv () =
    Printf.printf "Testing addv...\n";
    if testcase1_addv() && testcase2_addv() && testcase3_addv() && testcase4_addv() && testcase5_addv()  then
      print_endline "test_addv passed"
    else
      print_endline "test_addv failed";;
  
  let test_dot_prod () =
    Printf.printf "Testing dot_prod...\n";
    if testcase1_dot_prod() && testcase2_dot_prod() && testcase3_dot_prod()&& testcase4_dot_prod() && testcase5_dot_prod() then
      print_endline "test_dot_prod passed"
    else
      print_endline "test_dot_prod failed";;
  
  let test_inv () =
    Printf.printf "Testing inv...\n";
    if testcase1_inv() && testcase2_inv() && testcase3_inv() && testcase4_inv() && testcase5_inv() then
      print_endline "test_inv passed"
    else
      print_endline "test_inv failed";;
  
  let test_length () =
    Printf.printf "Testing length...\n";
    if testcase1_length() && testcase2_length() && testcase3_length() && testcase4_length() && testcase5_length() then
      print_endline "test_length passed"
    else
      print_endline "test_length failed";;
  
  let test_angle () =
    Printf.printf "Testing angle...\n";
    if testcase1_angle() && testcase2_angle() && testcase3_angle() && testcase4_angle() && testcase5_angle() then
      print_endline "test_angle passed"
    else
      print_endline "test_angle failed";;
  
  let () =
    test_create ();
    test_dim ();
    test_is_zero ();
    test_unit ();
    test_scale ();
    test_addv ();
    test_dot_prod ();
    test_inv ();
    test_length ();
    test_angle ();
    ;;  


(*
    Main focus of implementation was on correctness rather than efficiency. I have tried to use tail recursive functions wherever possible.
    But in addv, scale and inv, I have avoided using tail recursive functions, as a part of my design decision.
    Benefit of tail recursion would be that it would be more efficient in terms of memory usage, but it would make the code more complex and less readable along with worse time complexity (as I would have to reverse the list).
    I have used List.map in scale and inv, as it is more readable and efficient (time) than tail recursion.
    One added benefit in addv is that, it is easier to reason about correctness of the code.


    Proofs: (For all vectors u, v, w and scalar b and c)
    First, proving that addv u v = u + v
          Proof by induction on the dimension of the vector u (dimension of u = dimension of v)
          Base case: dimension of u = 1 (u = [u1], v = [v1])
              addv u v = addv [u1] [v1] = aux [u1] [v1] = (u1 + v1) :: aux [] [] // definition of addv and aux
              (u1 + v1) :: aux [] [] = [u1 + v1] :: [] = [u1 + v1] // definition of aux
              [u1 + v1] = u + v// definition of vector addition
          Induction hypothesis: Suppose addv u v = u + v for all vectors u and v of dimension n
          Inductive Step: Let u = [u1] :: u' and v = [v1] :: v' where u' and v' are vectors of dimension n
              addv u v = aux u v = (u1 + v1) :: aux u' v'  // definition of addv and aux
              aux u' v' = addv u' v' = u' + v' // induction hypothesis
              [u1 + v1] :: (aux u1' v1') = [u1 + v1] :: (addv u' v') = [u1 + v1] :: (u' + v') = u + v // definition of aux, addv and vector addition
          Therefore, addv u v = u + v for all vectors u and v by induction on the dimension of the vector u

    1. commutativity of vectors: u+v = v+u

        In above implementation add u v = u + v (Proof for this is done above)
        To prove u + v = v + u, we need to prove that addv u v = addv v u
          
        Proof by induction on the dimension of the vector u (dimension of u = dimension of v)
        Base case: dimension of u = 1 (u = [u1], v = [v1])
            addv u v = [u1] + [v1] = [u1 + v1] = [v1 + u1] = [v1] + [u1] = addv v u // definition of vector addition, Commutativity of + operator over floats, definition of addv
        Induction hypothesis: Suppose addv u v = addv v u for all vectors u and v of dimension n
        Inductive Step: Let u = [u1] :: u' and v = [v1] :: v' where u' and v' are vectors of dimension n
        addv u v = aux u v = [u1 + v1] :: aux u' v' = [u1 + v1] :: addv u' v' = [u1 + v1] :: addv v' u' // Induction hypothesis, definition of addv and aux
        [u1 + v1] :: addv v' u' = [v1 + u1] :: addv v' u' = addv v u // Commutativity of + operator over floats, definition of addv, vector addition
        Therefore, addv u v = addv v u for all vectors u and v by induction on the dimension of the vector u



    2. associativity of vectors: (u+v)+w = u+(v+w)

        To prove this, we need to prove that addv (addv u v) w = addv u (addv v w)
        Proof by induction on the dimension of the vector u (dimension of u = dimension of v = dimension of w)
        Base case: dimension of u = 1 (u = [u1], v = [v1], w = [w1])
            addv (addv u v) w = addv (addv [u1] [v1]) [w1] = addv [u1 + v1] [w1] = [u1 + v1 + w1] = [u1] + [v1 + w1] = addv [u1] [v1 + w1] = addv u (addv v w) // definition of vector addition, Associativity of + operator over floats, definition of addv
        Induction hypothesis: Suppose addv (addv u v) w = addv u (addv v w) for all vectors u, v and w of dimension n
        Inductive Step: Let u = [u1] :: u', v = [v1] :: v' and w = [w1] :: w' where u', v' and w' are vectors of dimension n
            addv (addv u v) w = addv ([u1 + v1] :: addv u' v') w = addv ([u1 + v1] :: addv u' v') ([w1] :: w') // definition of addv
            addv ([u1 + v1] :: addv u' v') ([w1] :: w') = [u1 + v1 + w1] :: addv (addv u' v') w' = [u1 + v1 + w1] :: addv u' (addv v' w') // Induction hypothesis, definition of addv
            [u1 + v1 + w1] :: addv u' (addv v' w') = addv ([u1] :: u') ([v1 + w1] :: addv v' w') = addv u ([v1 + w1] :: addv v' w') = addv u (addv v w) // definition of addv, vector addition
        Therefore, addv (addv u v) w = addv u (addv v w) for all vectors u, v and w by induction on the dimension of the vector u

    3. Identity of addition: u + 0 = u

        To prove this, we need to prove that addv u 0 = u // Here 0 is the zero vector
        Proof by induction on the dimension of the vector u
        Base case: dimension of u = 1 (u = [u1])
            addv u 0 = addv [u1] [0.0] = [u1 + 0.0] = [u1] = u // definition of vector addition, definition of addv, Identity of + operator over floats
        Induction hypothesis: Suppose addv u 0 = u for all vectors u of dimension n
        Inductive Step: Let u = [u1] :: u' where u' is a vector of dimension n
            addv u 0 = addv ([u1] :: u') ([0.0] :: 0) = [u1 + 0.0] :: addv u' 0 // definition of addv
            addv u' 0 = u' // Induction hypothesis
            [u1 + 0.0] :: u' = [u1] :: u' = u // definition of vector addition, Identity of + operator over floats
        Therefore, addv u 0 = u for all vectors u by induction on the dimension of the vector u

    4. Identity scalar: 1.u = u

        To prove this, we need to prove that scale 1 u = u
        Proof by induction on the dimension of the vector u
        Base case: dimension of u = 1 (u = [u1])
            scale 1 u = scale 1 [u1] = [1.0 * u1] = [u1] = u // definition of scale and correctness of function map
        Induction hypothesis: Suppose scale 1 u = u for all vectors u of dimension n
        Inductive Step: Let u = [u1] :: u' where u' is a vector of dimension n
            scale 1 u = List.map (fun y -> 1.0 * y) u = [1.0 * u1] :: List.map (fun y -> 1.0 * y) u' = [u1] :: (scale 1 u') // definition of scale and correctness of function map
            [u1] :: (scale 1 u') = [u1] :: u' // Induction hypothesis
            [u1] :: u' = u // Initial assumption
        Therefore, scale 1 u = u for all vectors u by induction on the dimension of the vector u

    5. Annihilator scalar: 0.u = 0

        To prove this, we need to prove that scale 0 u = 0
        Proof by induction on the dimension of the vector u
        Base case: dimension of u = 1 (u = [u1])
            scale 0 u = scale 0 [u1] = [0.0 * u1] = [0.0] = 0 // definition of scale and correctness of function map
        Induction hypothesis: Suppose scale 0 u = 0 for all vectors u of dimension n
        Inductive Step: Let u = [u1] :: u' where u' is a vector of dimension n
            scale 0 u = List.map (fun y -> 0.0 * y) u = [0.0 * u1] :: List.map (fun y -> 0.0 * y) u' = [0.0] :: (scale 0 u') // definition of scale and correctness of function map
            [0.0] :: (scale 0 u') = [0.0] :: 0 = 0 // Induction hypothesis
        Therefore, scale 0 u = 0 for all vectors u by induction on the dimension of the vector u

    6. Additive Inverse: v + (- v) = 0

        To prove this, we need to prove addv u (inv u) = 0
        Proof by induction on the dimension of the vector u
        Base case: dimension of u = 1 (u = [u1])
            addv u (inv u) = addv [u1] (inv [u1]) = addv [u1] [-1.0 * u1] // definition of inv and correctness of function map
            addv [u1] [-1.0 * u1] = [u1 + (-1.0) * u1] = [0] = 0 // definition of addv
        Induction hypothesis: Suppose addv u (inv v) = 0 for all vectors of dimension n
        Inductive Step: Let u = [u1] :: u' where u' is a vector of dimension n
            addv u (inv u) = addv ([u1] :: u') (List.map (fun y -> -1.0 * y) u) = addv ([u1] :: u') ([-1.0 * u1] :: List.map (fun y -> -1.0 * y) u') = addv ([u1] :: u') ([-1.0 * u1] :: (inv u')) // definition of inv, correctness of function map
            addv ([u1] :: u') ([-1.0 * u1] :: (inv u')) = [u1 + (-1.0) * u1] :: (addv u' (inv u')) = [0.0] :: (addv u' (inv u')) // definition of addv
            0.0 :: (addv u' (inv u')) = [0.0] :: 0 // Induction hypothesis
            0.0 :: 0 = 0
        
        Therefore, addv u (inv u) = 0 for all vectors u by induction on the dimension of the vector u


    7. Scalar product combination: b.(c.v) = (b.c).v

        To prove this, we need to prove scale b (scale c u) = scale (b * c) u
        Proof by induction on the dimension of the vector u
        Base case: dimension of u = 1 (u = [u1])
            scale b (scale c u) = scale b (scale c [u1]) = scale b [c * u1] //definition of scale and correctness of function map
            scale b [c * u1] = [b * (c * u1)] // correctness of function map
            [b * (c * u1)] = [(b * c) * u1] // associativity of operator * over floats
            [(b * c) * u1] = scale (b * c) [u1] = scale (b*c) u //definition of scale
        Induction hypothesis: Suppose scale b (scale u) = scale (b * c) u for all vectors of dimension n
        Inductive Step: Let u = [u1] :: u' where u' is a vector of dimension n
        scale b (scale c u) = scale b (scale c ([u1] :: u')) = scale b ([c * u1] :: (scale c u')) // definition of scale, correctness of function map
        scale b ([c * u1] :: (scale c u')) = [b * c * u1] :: (scale b (scale c u')) // definition of scale
        [b * c * u1] :: (scale b (scale c u')) = [b * c * u1] :: scale (b*c) u' // Induction hypothesis
        [b * c * u1] :: scale (b*c) u' = scale (b*c) ([u1] :: u') = scale (b*c) u // definition of scale
        
        Therefore, scale b (scale c u) = scale (b * c) u for all vectors u by induction on the dimension of the vector u
    
    8. Scalar sum-product distribution: (b + c).v = b.v + c.v

        To prove this, we need to prove scale (b + c) u = addv (scale b u) (scale c u)
        Proof by induction on the dimension of the vector u
        Base case: dimension of u = 1 (u = [u1])
            scale (b + c) u = scale (b + c) [u1] = [(b+c) * u1] // definition of scale
            [(b+c) * u1] = [b*u1 + c*u1] // distributivity for floats
            [b*u1 + c*u1] = [b*u1] + [c*u1] // vector addition
            [b*u1] + [c*u1] = addv [b*u1] [c*u1] // definition of addv
            addv [b*u1] [c*u1] = addv (scale b [u1]) (scale c [u1]) = addv (scale b u) (scale c u) // definition of addv
        Induction hypothesis: Suppose scale (b + c) u = addv (scale b u) (scale c u) for all vectors of dimension n
        Inductive Step: Let u = [u1] :: u' where u' is a vector of dimension n
        scale (b + c) u = scale (b + c) ([u1] :: u') = [(b+c) * u1] :: (scale (b + c) u') // definition of scale
        [(b+c) * u1] :: (scale (b + c) u') = [b*u1 + c*u1] :: (scale (b + c) u') // distributivity for floats
        [b*u1 + c*u1] :: (scale (b + c) u') = [b*u1 + c*u1] :: (addv (scale b u') (scale c u')) // Induction hypothesis
        [b*u1 + c*u1] :: (addv (scale b u') (scale c u')) = addv ([b*u1] :: (scale b u')) ([c*u1] :: (scale c u')) // definition of addv
        addv ([b*u1] :: (scale b u')) ([c*u1] :: (scale c u')) = addv (scale b ([u1] :: u')) (scale c ([u1] :: u')) = addv (scale b u) (scale c u) // definition of scale
        
        Therefore, scale (b + c) u = addv (scale b u) (scale c u) for all vectors u by induction on the dimension of the vector u

    9. Scalar  Distribution over vector sums: b.(u + v) = b.u + b.v

        To prove this, we need to prove scale b (addv u v) = addv (scale b u) (scale b v)
        Proof by induction on the dimension of the vector u (dimension of vector u = dimension of vector v)
        Base case: dimension of u = 1 (u = [u1], v = [v1])
              scale b (addv u v) = scale b (addv [u1] [v1]) = scale b ([u1 + v1]) // definition of addv
              scale b ([u1 + v1]) = [b* (u1 + v1)] // definition of scale
              [b* (u1 + v1)] = [b*u1 + b*v1] // distributivity for floats
              [b*u1 + b*v1] = [b*u1] + [b*v1] // vector addition
              [b*u1] + [b*v1] = addv [b*u1] [b*v1] // definition of addv
              addv [b*u1] [b*v1] = addv (scale b [u1]) (scale b [v1]) = addv (scale b u) (scale b v) // definition of scale
        Induction hypothesis: Suppose scale b (addv u v) = addv (scale b u) (scale b v) for all vectors of dimension n
        Inductive Step: Let u = [u1] :: u', v = [v1] :: v' where u' and v' are vectors of dimension n
        scale b (addv u v) = scale b (addv ([u1] :: u') ([v1] :: v')) = scale b ([u1 + v1] :: (addv u' v')) //definition of addv
        scale b ([u1 + v1] :: (addv u' v')) = [b* (u1 + v1)] :: (scale b (addv u' v')) // definition of scale
        [b* (u1 + v1)] :: (scale b (addv u' v')) = [b * (u1 + v1)] :: (addv (scale b u') (scale b v')) // Induction hypothesis
        [b * (u1 + v1)] :: (addv (scale b u') (scale b v')) = [b*u1 + b*v1] :: (addv (scale b u') (scale b v')) // distributivity for flaots
        [b*u1 + b*v1] :: (addv (scale b u') (scale b v')) = addv ([b*u1] :: (scale b u')) ([b*v1] :: (scale b v')) // definition of addv
        addv ([b*u1] :: (scale b u')) ([b*v1] :: (scale b v')) = addv (scale b ([u1] :: u')) (scale b ([v1] :: v')) // definition of scale
        addv (scale b ([u1] :: u')) (scale b ([v1] :: v')) = addv (scale b u) (scale b v)

        Therefore, scale b (addv u v) = addv (scale b u) (scale b v) for all vectors u, v by induction on the dimension of the vectors u, v

    Other Properties:
    1. Commutativity for dot product: u.v = v.u
    2. -(u + v) = -u + -v
    3. length of 2*(u) = 2*(length of u)
    4. angle between u and v = angle between v and u

    1. Commutativity for dot product: u.v and v.u

        To prove this, we need to prove dot_prod u v = dot_prod v u
        Proof by induction on the dimension of the vector u (dimension of u = dimension of v)
        Base case: dimension of u = 1 (u = [u1], v = [v1])
            dot_prod u v = dot_prod [u1] [v1] = aux [u1] [v1] 0.0 // definition of dot_prod and aux
            aux [u1] [v1] 0.0 = aux [] [] (u1 * v1) // definition of aux
            aux [] [] (u1 * v1) = u1 * v1 = v1 * u1 = aux [] [] (v1 * u1) // commutativity of * operator over floats
            aux [] [] (v1 * u1) = aux [v1] [u1] 0.0 = dot_prod [v1] [u1] // definition of aux
            dot_prod [v1] [u1] = dot_prod v u // definition of dot_prod
            
        
        Induction Hypothesis: Suppose dot_prod u v = dot_prod v u for all vectors u and v of dimension n
        Induction Step: Let u = [u1] :: u' and v = [v1] :: v' where u' and v' are vectors of dimension n
            dot_prod u v = aux u v 0.0 = aux ([u1] :: u') ([v1] :: v') 0.0 // definition of dot_prod and aux
            aux ([u1] :: u') ([v1] :: v') 0.0 = aux u' v' (u1 * v1) // definition of aux
            aux u' v' 0.0 = dot_prod u' v' // definition of dot_prod
            dot_prod u' v' = dot_prod v' u' // Induction hypothesis
            aux u' v' (u1 * v1) = u1 * v1 + aux u' v' 0.0 = u1 * v1 + dot_prod u' v' = u1 * v1 + dot_prod v' u' // definition of aux
            u1 * v1 + dot_prod v' u' = v1 * u1 + aux v' u' 0.0 // commutativity of * operator over floats, definition of aux
            v1 * u1 + aux v' u' 0.0 = aux ([v1] :: v') ([u1] :: u') 0.0 = dot_prod v u // definition of aux, dot_prod

        Therefore, dot_prod u v = dot_prod v u for all vectors u and v by induction on the dimension of the vector u
    2. -(u + v) = (-u) + (-v)

          To prove this, we need to prove inv (addv u v) = addv (inv u) (inv v)
          Proof by induction on the dimension of the vector u (dimension of u = dimension of v)
          Base case: dimension of u = 1 (u = [u1], v = [v1])
              inv (addv u v) = inv (addv [u1] [v1]) = inv [u1 + v1] // definition of addv
              inv [u1 + v1] = [-1.0 * (u1 + v1)] = [-1.0 * u1 + -1.0 * v1] = [-1.0 * u1] + [-1.0 * v1] = addv [-1.0 * u1] [-1.0 * v1] = addv (inv [u1]) (inv [v1]) // definition of inv and addv
          Induction hypothesis: Suppose inv (addv u v) = addv (inv u) (inv v) for all vectors u and v of dimension n
          Induction Step: Let u = [u1] :: u' and v= [v1] :: v' where u' and v' are vectors of dimension n
              inv (addv u v) = inv (addv ([u1] :: u') ([v1] :: v')) = inv ([u1 + v1] :: addv u' v') // definition of addv
              inv ([u1 + v1] :: addv u' v') = [-1.0 * (u1 + v1)] :: inv (addv u' v') // definition of inv
              [-1.0 * (u1 + v1)] :: inv (addv u' v') = [-1.0 * u1 + -1.0 * v1] :: inv (addv u' v') // distributivity of * operator over floats
              [-1.0 * u1 + -1.0 * v1] :: inv (addv u' v') =  [-1.0 * u1 + -1.0 * v1] :: addv (inv u') (inv v') // Induction hypothesis
              [-1.0 * u1 + -1.0 * v1] :: addv (inv u') (inv v') = addv ([(-1.0 * u1)] :: (inv u')) ([(-1.0 * v1)] :: (inv v')) // definition of addv
              addv ([(-1.0 * u1)] :: (inv u')) ([(-1.0 * v1)] :: (inv v')) = addv (inv ([u1] :: u')) (inv ([v1]::v')) // definition of inv and addv
              addv (inv ([u1] :: u')) (inv ([v1]::v')) = addv (inv u) (inv v) // definition of inv
          Therefore, inv (addv u v) = addv (inv u) (inv v) for all vectors u and v by induction on the dimension of the vector u

    3. length of b*(u) = abs(b)*(length of u)
          // abs(b) is abosolute value of b
          To prove this, we need to prove length (scale b u) = b * (length u)
          Proof by induction on the dimension of the vector u
          Base case: dimension of u = 1 (u = [u1])
              length (scale b u) = length (scale b [u1]) = length [b * u1] // definition of scale
              length [b * u1] = sqrt (dot_prod [b * u1] [b * u1]) // definition of length
              sqrt (dot_prod [b * u1] [b * u1]) = sqrt (b * u1 * b * u1) // definition of dot_prod
              sqrt (b * u1 * b * u1) = sqrt (b * b * u1 * u1) // associativity of * operator over floats
              sqrt (b * b * u1 * u1) = sqrt (b * b) * sqrt (u1 * u1) // property of sqrt
              sqrt (b * b) * sqrt (u1 * u1) = abs(b) * sqrt (u1 * u1) = abs(b) * length [u1] // definition of length
              abs(b) * length [u1] = abs(b) * length u 
          Induction hypothesis: Suppose length (scale b u) = abs(b) * length u for all vectors u of dimension n
          Inductive Step: Let u = [u1] :: u' where u' is a vector of dimension n
              length (scale b u) = length (scale b ([u1] :: u')) = length ([b * u1] :: scale b u') // definition of scale
              length ([b * u1] :: scale b u') = sqrt (dot_prod ([b * u1] :: scale b u') ([b * u1] :: scale b u')) // definition of length
              sqrt (dot_prod ([b * u1] :: scale b u') ([b * u1] :: scale b u')) = sqrt (b * u1 * b * u1 + dot_prod (scale b u') (scale b u')) // definition of dot_prod
              sqrt (b * u1 * b * u1 + dot_prod (scale b u') (scale b u')) = sqrt (b * b * u1 * u1 + dot_prod (scale b u') (scale b u')) // associativity of * operator over floats
              dot_prod (scale b u') (scale b u') = length (scale b u') * length (scale b u') // definition of length
              length (scale b u') * length (scale b u') = abs(b) * length u' * abs(b) * length u' // Induction hypothesis
              abs(b() * length u' * abs(b) * length u' = abs(b) * abs(b) * length u' * length u' // associativity of * operator over floats
              sqrt (b * b * u1 * u1 + dot_prod (scale b u') (scale b u')) = sqrt (b * b * u1 * u1 + b * b * length u' * length u') // proved above
              sqrt (b * b * u1 * u1 + b * b * length u' * length u') = sqrt (b * b) * sqrt (u1 * u1 + length u' * length u') // property of sqrt
              sqrt (b * b) * sqrt (u1 * u1 + length u' * length u') = abs(b) * sqrt (u1 * u1 + dot_prod u' u')  // definition of length
              abs(b) * sqrt (u1 * u1 + dot_prod u' u') = abs(b) * sqrt (dot_prod ([u1] :: u') ([u1] :: u')) // definition of dot_prod
              abs(b) * sqrt (dot_prod ([u1] :: u') ([u1] :: u')) = abs(b) * length ([u1] :: u') // definition of length
              abs(b) * length ([u1] :: u') = abs(b) * length u

          Therefore, length (scale b u) = abs(b) * length u for all vectors u by induction on the dimension of the vector u

    4. angle between u and v = angle between v and u

        To prove this we need to prove that angle u v = angle v u
        angle u v = acos (dot_prod u v / (length u * length v)) // definition of angle
        acos (dot_prod u v / (length u * length v)) = acos (dot_prod v u / (length v * length u)) // commutativity of dot_prod, commutativity of * operator over floats
        acos (dot_prod v u / (length v * length u)) = angle v u // definition of angle

        Therefore, angle u v = angle v u for all vectors u and v (Here I used the above proven property that dot_prod u v = dot_prod v u)
*)