type deb = Vd of string | Index of int | Absd of deb | Appd of deb * deb
type lam = V of string | Abs of string * lam | App of lam * lam

let rec pos x l = match l with
  | [] -> raise Not_found
  | h::t -> if x = h then 0 else 1 + pos x t

let rec shift c d e = match e with
  | Vd x -> Vd x
  | Index n -> if n < c then Index n else Index (n + d)
  | Absd e -> Absd (shift (c + 1) d e)
  | Appd (e1, e2) -> Appd (shift c d e1, shift c d e2)

let rec substdeb e e' j = match e with
  | Vd x -> Vd x
  | Index(i) -> if i = j then e' else Index(i)
  | Absd e1 -> Absd (substdeb e1 (shift 0 1 e') (j + 1))
  | Appd (e1, e2) -> Appd (substdeb e1 e' j, substdeb e2 e' j)

exception NotIn
let rec reduce e = match e with
  | Vd x -> Vd x
  | Index _n -> raise NotIn
  | Absd e1 -> Absd e1
  | Appd (Absd(e1), e2) -> (let c1 = (shift 0 (-1) (substdeb e1 (shift 0 1 e2) 0)) in 
    reduce c1)
  | Appd (Vd x, e2) -> Appd (Vd x, reduce e2)
  | Appd (e1, e2) -> reduce (Appd (reduce e1, e2))

let rec lamify_help e s d =
  match e with
  | Vd x -> V x
  | Index n -> V (List.nth s n)
  | Absd e1 -> let new_x = "x" ^ string_of_int d in
    let new_s = new_x :: s in
    Abs (new_x, lamify_help e1 new_s (d + 1))
  | Appd (e1, e2) -> App (lamify_help e1 s d, lamify_help e2 s d)

let lamify e = lamify_help e [] 0

let deb1= Appd (Absd (Index 0), Absd (Absd (Index 1)))

let lam1 = lamify deb1

let rec lam_to_string l = match l with
  | V x -> x
  | Abs (x, l1) -> "λ" ^ x ^ "." ^ lam_to_string l1
  | App (l1, l2) -> "(" ^ lam_to_string l1 ^ " " ^ lam_to_string l2 ^ ")"
let rec deb_to_string d = match d with
  | Vd x -> x
  | Index n -> string_of_int n
  | Absd e -> "λ" ^ deb_to_string e
  | Appd (e1, e2) -> "(" ^ deb_to_string e1 ^ " " ^ deb_to_string e2 ^ ")"
   
