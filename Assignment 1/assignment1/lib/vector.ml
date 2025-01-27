open Sexplib.Std
type vector = float list [@@deriving sexp]

exception DimensionError

let truncate x decimals=
  let multiplier = 10.0 ** (float_of_int decimals) in
  (floor (x *. multiplier)) /. multiplier;;

let create n x = 
  if n < 1 then
    raise  DimensionError
  else
    let rec aux n acc = 
      if n = 0 then acc else aux (n-1) (x::acc)
    in
    aux n []
  ;;

let dim v = 
  let length = List.length v in
  if length < 1 then
    raise DimensionError
  else
    length
  
  ;;

let is_zero v = 
  if (dim v) < 1 then
    raise DimensionError
  else
  List.for_all (fun x -> x = 0.0) v;;

let unit n j =
  if n < 1 || j < 1 || j > n then
    raise DimensionError
  else
  let rec aux n j acc =
    if n = 0 then acc else aux (n-1) j ((if n = j then 1.0 else 0.0)::acc)
  in
  aux n j [];;

let scale x v = 
  if (dim v) < 1 then
    raise DimensionError
  else
  List.map (fun y -> x *. y) v;;

let addv v1 v2 = 
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
    

let dot_prod v1 v2 = 
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

let inv v =
  if (dim v) < 1 then
    raise DimensionError
  else 
  List.map (fun x -> -.x) v;;

let length v = 
  if (dim v) < 1 then
    raise DimensionError
  else
  sqrt (dot_prod v v);;

let angle v1 v2 =
  let len_v1 = length v1 in
  let len_v2 = length v2 in
  if len_v1 = 0.0 || len_v2 = 0.0 then
    raise DimensionError  
  else
    acos ((dot_prod v1 v2) /. (len_v1 *. len_v2));;



(*
    Proofs: (For all vectors u, v, w and scalar b and c)
    First, proving that addv u v = u + v
          Proof by induction on the length of the vector u (length of u = length of v)
          Base case: length of u = 1 (u = [u1], v = [v1])
              addv u v = addv [u1] [v1] = aux [u1] [v1] = (u1 + v1) :: aux [] [] // definition of addv and aux
              (u1 + v1) :: aux [] [] = [u1 + v1] :: [] = [u1 + v1] // definition of aux
              [u1 + v1] = u + v// definition of vector addition
          Induction hypothesis: Suppose addv u v = u + v for all vectors u and v of length n
          Inductive Step: Let u = [u1] :: u' and v = [v1] :: v' where u' and v' are vectors of length n
              addv u v = aux u v = (u1 + v1) :: aux u' v'  // definition of addv and aux
              aux u' v' = addv u' v' = u' + v' // induction hypothesis
              [u1 + v1] :: (aux u1' v1') = [u1 + v1] :: (addv u' v') = [u1 + v1] :: (u' + v') = u + v // definition of aux, addv and vector addition
          Therefore, addv u v = u + v for all vectors u and v by induction on the length of the vector u

    1. commutativity of vectors: u+v = v+u

        In above implementation add u v = u + v (Proof for this is done above)
        To prove u + v = v + u, we need to prove that addv u v = addv v u
          
        Proof by induction on the length of the vector u (length of u = length of v)
        Base case: length of u = 1 (u = [u1], v = [v1])
            addv u v = [u1] + [v1] = [u1 + v1] = [v1 + u1] = [v1] + [u1] = addv v u // definition of vector addition, Commutativity of + operator over floats, definition of addv
        Induction hypothesis: Suppose addv u v = addv v u for all vectors u and v of length n
        Inductive Step: Let u = [u1] :: u' and v = [v1] :: v' where u' and v' are vectors of length n
        addv u v = aux u v = [u1 + v1] :: aux u' v' = [u1 + v1] :: addv u' v' = [u1 + v1] :: addv v' u' // Induction hypothesis, definition of addv and aux
        [u1 + v1] :: addv v' u' = [v1 + u1] :: addv v' u' = addv v u // Commutativity of + operator over floats, definition of addv
        Therefore, addv u v = addv v u for all vectors u and v by induction on the length of the vector u



    2. associativity of vectors: (u+v)+w = u+(v+w)

        To prove this, we need to prove that addv (addv u v) w = addv u (addv v w)
        Proof by induction on the length of the vector u (length of u = length of v = length of w)
        Base case: length of u = 1 (u = [u1], v = [v1], w = [w1])
            addv (addv u v) w = addv (addv [u1] [v1]) [w1] = addv [u1 + v1] [w1] = [u1 + v1 + w1] = [u1] + [v1 + w1] = addv [u1] [v1 + w1] = addv u (addv v w) // definition of vector addition, Associativity of + operator over floats, definition of addv
        Induction hypothesis: Suppose addv (addv u v) w = addv u (addv v w) for all vectors u, v and w of length n
        Inductive Step: Let u = [u1] :: u', v = [v1] :: v' and w = [w1] :: w' where u', v' and w' are vectors of length n
            addv (addv u v) w = addv ([u1 + v1] :: addv u' v') w = addv ([u1 + v1] :: addv u' v') ([w1] :: w') // definition of addv
            addv ([u1 + v1] :: addv u' v') ([w1] :: w') = [u1 + v1 + w1] :: addv (addv u' v') w' = [u1 + v1 + w1] :: addv u' (addv v' w') // Induction hypothesis, definition of addv
            [u1 + v1 + w1] :: addv u' (addv v' w') = addv ([u1] :: u') ([v1 + w1] :: addv v' w') = addv u ([v1 + w1] :: addv v' w') = addv u (addv v w) // definition of addv
        Therefore, addv (addv u v) w = addv u (addv v w) for all vectors u, v and w by induction on the length of the vector u

    3. Identity of addition: u + 0 = u

        To prove this, we need to prove that addv u 0 = u // Here 0 is the zero vector
        Proof by induction on the length of the vector u
        Base case: length of u = 1 (u = [u1])
            addv u 0 = addv [u1] [0.0] = [u1 + 0.0] = [u1] = u // definition of vector addition, definition of addv, Identity of + operator over floats
        Induction hypothesis: Suppose addv u 0 = u for all vectors u of length n
        Inductive Step: Let u = [u1] :: u' where u' is a vector of length n
            addv u 0 = addv ([u1] :: u') ([0.0] :: 0) = [u1 + 0.0] :: addv u' 0 // definition of addv
            addv u' 0 = u' // Induction hypothesis
            [u1 + 0.0] :: u' = [u1] :: u' = u // definition of vector addition, Identity of + operator over floats
        Therefore, addv u 0 = u for all vectors u by induction on the length of the vector u

    4. Identity scalar: 1.u = u

        To prove this, we need to prove that scale 1 u = u
        Proof by induction on the length of the vector u
        Base case: length of u = 1 (u = [u1])
            scale 1 u = scale 1 [u1] = [1.0 * u1] = [u1] = u // definition of scale and correctness of function map
        Induction hypothesis: Suppose scale 1 u = u for all vectors u of length n
        Inductive Step: Let u = [u1] :: u' where u' is a vector of length n
            scale 1 u = List.map (fun y -> 1.0 * y) u = [1.0 * u1] :: List.map (fun y -> 1.0 * y) u' = [u1] :: (scale 1 u') // definition of scale and correctness of function map
            [u1] :: (scale 1 u') = [u1] :: u' // Induction hypothesis
            [u1] :: u' = u // Initial assumption
        Therefore, scale 1 u = u for all vectors u by induction on the length of the vector u

        

    5. Annihilator scalar: 0.u = 0

        To prove this, we need to prove that scale 0 u = 0
        Proof by induction on the length of the vector u
        Base case: length of u = 1 (u = [u1])
            scale 0 u = scale 0 [u1] = [0.0 * u1] = [0.0] = 0 // definition of scale and correctness of function map
        Induction hypothesis: Suppose scale 0 u = 0 for all vectors u of length n
        Inductive Step: Let u = [u1] :: u' where u' is a vector of length n
            scale 0 u = List.map (fun y -> 0.0 * y) u = [0.0 * u1] :: List.map (fun y -> 0.0 * y) u' = [0.0] :: (scale 0 u') // definition of scale and correctness of function map
            [0.0] :: (scale 0 u') = [0.0] :: 0 = 0 // Induction hypothesis
        Therefore, scale 0 u = 0 for all vectors u by induction on the length of the vector u

    6. Additive Inverse: v + (- v) = 0

        To prove this, we need to prove addv u (inv u) = 0
        Proof by induction on the length of the vector u
        Base case: length of u = 1 (u = [u1])
            addv u (inv u) = addv [u1] (inv [u1]) = addv [u1] [-1.0 * u1] // definition of inv and correctness of function map
            addv [u1] [-1.0 * u1] = [u1 + (-1.0) * u1] = [0] = 0 // definition of addv
        Induction hypothesis: Suppose addv u (inv v) = 0 for all vectors of length n
        Inductive Step: Let u = [u1] :: u' where u' is a vector of length n
            addv u (inv u) = addv ([u1] :: u') (List.map (fun y -> -1.0 * y) u) = addv ([u1] :: u') ([-1.0 * u1] :: List.map (fun y -> -1.0 * y) u') = addv ([u1] :: u') ([-1.0 * u1] :: (inv u')) // definition of inv, correctness of function map
            addv ([u1] :: u') ([-1.0 * u1] :: (inv u')) = [u1 + (-1.0) * u1] :: (addv u' (inv u')) = [0.0] :: (addv u' (inv u')) // definition of addv
            [0.0] :: (addv u' (inv u')) = [0.0] :: 0 // Induction hypothesis
            [0.0] :: 0 = 0
        
        Therefore, addv u (inv u) = 0 for all vectors u by induction on the length of the vector u


    7. Scalar product combination: b.(c.v) = (b.c).v

        To prove this, we need to prove scale b (scale c u) = scale (b * c) u
        Proof by induction on the length of the vector u
        Base case: length of u = 1 (u = [u1])
            scale b (scale c u) = scale b (scale c [u1]) = scale b [c * u1] //definition of scale and correctness of function map
            scale b [c * u1] = [b * (c * u1)] // correctness of function map
            [b * (c * u1)] = [(b * c) * u1] // associativity of operator * over floats
            [(b * c) * u1] = scale (b * c) [u1] = scale (b*c) u //definition of scale
        Induction hypothesis: Suppose scale b (scale u) = scale (b * c) u for all vectors of length n
        Inductive Step: Let u = [u1] :: u' where u' is a vector of length n
        scale b (scale c u) = scale b (scale c ([u1] :: u')) = scale b ([c * u1] :: (scale c u')) // definition of scale, correctness of function map
        scale b ([c * u1] :: (scale c u')) = [b * c * u1] :: (scale b (scale c u')) // definition of scale
        [b * c * u1] :: (scale b (scale c u')) = [b * c * u1] :: scale (b*c) u' // Induction hypothesis
        [b * c * u1] :: scale (b*c) u' = scale (b*c) ([u1] :: u') = scale (b*c) u // definition of scale
        
        Therefore, scale b (scale c u) = scale (b * c) u for all vectors u by induction on the length of the vector u
    
    8. Scalar sum-product distribution: (b + c).v = b.v + c.v

        To prove this, we need to prove scale (b + c) u = addv (scale b u) (scale c u)
        Proof by induction on the length of the vector u
        Base case: length of u = 1 (u = [u1])
            scale (b + c) u = scale (b + c) [u1] = [(b+c) * u1] // definition of scale
            [(b+c) * u1] = [b*u1 + c*u1] // distributivity for floats
            [b*u1 + c*u1] = [b*u1] + [c*u1] // vector addition
            [b*u1] + [c*u1] = addv [b*u1] [c*u1] // definition of addv
            addv [b*u1] [c*u1] = addv (scale b [u1]) (scale c [u1]) = addv (scale b u) (scale c u) // definition of addv
        Induction hypothesis: Suppose scale (b + c) u = addv (scale b u) (scale c u) for all vectors of length n
        Inductive Step: Let u = [u1] :: u' where u' is a vector of length n
        scale (b + c) u = scale (b + c) ([u1] :: u') = [(b+c) * u1] :: (scale (b + c) u') // definition of scale
        [(b+c) * u1] :: (scale (b + c) u') = [b*u1 + c*u1] :: (scale (b + c) u') // distributivity for floats
        [b*u1 + c*u1] :: (scale (b + c) u') = [b*u1 + c*u1] :: (addv (scale b u') (scale c u')) // Induction hypothesis
        [b*u1 + c*u1] :: (addv (scale b u') (scale c u')) = addv ([b*u1] :: (scale b u')) ([c*u1] :: (scale c u')) // definition of addv
        addv ([b*u1] :: (scale b u')) ([c*u1] :: (scale c u')) = addv (scale b ([u1] :: u')) (scale c ([u1] :: u')) = addv (scale b u) (scale c u) // definition of scale
        
        Therefore, scale (b + c) u = addv (scale b u) (scale c u) for all vectors u by induction on the length of the vector u

    9. Scalar  Distribution over vector sums: b.(u + v) = b.u + b.v

        To prove this, we need to prove scale b (addv u v) = addv (scale b u) (scale b v)
        Proof by induction on the length of the vector u (length of vector u = length of vector v)
        Base case: length of u = 1 (u = [u1], v = [v1])
              scale b (addv u v) = scale b (addv [u1] [v1]) = scale b ([u1 + v1]) // definition of addv
              scale b ([u1 + v1]) = [b* (u1 + v1)] // definition of scale
              [b* (u1 + v1)] = [b*u1 + b*v1] // distributivity for floats
              [b*u1 + b*v1] = [b*u1] + [b*v1] // vector addition
              [b*u1] + [b*v1] = addv [b*u1] [b*v1] // definition of addv
              addv [b*u1] [b*v1] = addv (scale b [u1]) (scale b [v1]) = addv (scale b u) (scale b v) // definition of scale
        Induction hypothesis: Suppose scale b (addv u v) = addv (scale b u) (scale b v) for all vectors of length n
        Inductive Step: Let u = [u1] :: u', v = [v1] :: v' where u' and v' are vectors of length n
        scale b (addv u v) = scale b (addv ([u1] :: u') ([v1] :: v')) = scale b ([u1 + v1] :: (addv u' v')) //definition of addv
        scale b ([u1 + v1] :: (addv u' v')) = [b* (u1 + v1)] :: (scale b (addv u' v')) // definition of scale
        [b* (u1 + v1)] :: (scale b (addv u' v')) = [b * (u1 + v1)] :: (addv (scale b u') (scale b v')) // Induction hypothesis
        [b * (u1 + v1)] :: (addv (scale b u') (scale b v')) = [b*u1 + b*v1] :: (addv (scale b u') (scale b v')) // distributivity for flaots
        [b*u1 + b*v1] :: (addv (scale b u') (scale b v')) = addv ([b*u1] :: (scale b u')) ([b*v1] :: (scale b v')) // definition of addv
        addv ([b*u1] :: (scale b u')) ([b*v1] :: (scale b v')) = addv (scale b ([u1] :: u')) (scale b ([v1] :: v')) // definition of scale
        addv (scale b ([u1] :: u')) (scale b ([v1] :: v')) = addv (scale b u) (scale b v)

        Therefore, scale b (addv u v) = addv (scale b u) (scale b v) for all vectors u, v by induction on the length of the vectors u, v
        




    

            


        


          



            
    
        
*)


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

let testcase1_dot_prod = fun() -> truncate (dot_prod [1.0; 2.0; 3.0] [4.0; 5.0; 6.0]) 2 = 32.0;;
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






