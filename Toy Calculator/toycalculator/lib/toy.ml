type myBool = T | F;;
type values = N of int | B of bool;;
type exp = Num of int | B1 of myBool | Plus of exp*exp | Times of exp*exp | And of exp*exp | Or of exp*exp | Not of exp
| Eq of exp*exp | Gt of exp*exp;;
type opcode = LDN of int | LDB of myBool | PLUS | TIMES | AND | OR | NOT | EQ | GT ;;



exception Stuck;;
exception Wrong;;

let myBool2bool b = match b with
| T -> true
| F -> false;;

let bool2myBool b = match b with
| true -> T
| false -> F;;

let myAnd b1 b2 = match b1 with
| T -> b2
| F -> F;;

let myOr b1 b2 = match b1 with
| T -> T
| F -> b2;;

let myNot b = match b with
| T -> F
| F -> T;;

let rec size e = match e with
| Num _ -> 1
| B1 _ -> 1
| Plus (e1, e2) -> 1 + size e1 + size e2
| Times (e1, e2) -> 1 + size e1 + size e2 
| And (e1, e2) -> 1 + size e1 + size e2
| Or (e1, e2) -> 1 + size e1 + size e2
| Not e1 -> 1 + size e1
| Eq (e1, e2) -> 1 + size e1 + size e2
| Gt (e1, e2) -> 1 + size e1 + size e2;;


let rec ht e = match e with
| Num _ -> 0
| B1 _ -> 0
| Plus (e1, e2) -> 1 + max (ht e1) (ht e2)
| Times (e1, e2) -> 1 + max (ht e1) (ht e2)
| And (e1, e2) -> 1 + max (ht e1) (ht e2)
| Or (e1, e2) -> 1 + max (ht e1) (ht e2)
| Not e1 -> 1 + ht e1
| Eq (e1, e2) -> 1 + max (ht e1) (ht e2)
| Gt (e1, e2) -> 1 + max (ht e1) (ht e2);;


let rec posttrav e = match e with
| Num n -> Int.to_string n
| Plus (e1, e2) -> posttrav e1 ^ " " ^ posttrav e2 ^ " +"
| Times (e1, e2) -> posttrav e1 ^ " " ^ posttrav e2 ^ " *" 
| And (e1, e2) -> posttrav e1 ^ " " ^ posttrav e2 ^ " &"
| Or (e1, e2) -> posttrav e1 ^ " " ^ posttrav e2 ^ " |"
| Not e1 -> posttrav e1 ^ " !"
| Eq (e1, e2) -> posttrav e1 ^ " " ^ posttrav e2 ^ " ="
| Gt (e1, e2) -> posttrav e1 ^ " " ^ posttrav e2 ^ " >"
| B1 b -> if myBool2bool b then "T" else "F";;
;;

let rec eval e = match e with
  Num n -> N n
  | B1 b -> B (myBool2bool b)
  | Plus (e1, e2) -> (match eval e1, eval e2 with
    | N n1, N n2 -> N (n1 + n2)
    | _, _ -> raise Wrong)
  | Times (e1, e2) -> (match eval e1, eval e2 with
    | N n1, N n2 -> N (n1 * n2)
    | _, _ -> raise Wrong)
  | And (e1, e2) -> ( match eval e1, eval e2 with
    | B b1, B b2 -> B (b1 && b2)
    | _, _ -> raise Wrong)
  | Or (e1, e2) -> (match eval e1, eval e2 with
    | B b1, B b2 -> B (b1 || b2)
    | _, _ -> raise Wrong)
  | Not e1 -> (match eval e1 with
    | B b1 -> B (not b1)
    | _ -> raise Wrong)
  | Eq (e1, e2) ->( match eval e1, eval e2 with
    | N n1, N n2 -> B (n1 = n2)
    | _, _ -> raise Wrong)
  | Gt (e1, e2) -> (match eval e1, eval e2 with
    | N n1, N n2 -> B (n1 > n2)
    | _, _ -> raise Wrong)

;;

let rec compile e = match e with
Num n -> [LDN n]
| B1 b -> [LDB b]
| Plus (e1, e2) -> compile e1 @ compile e2 @ [PLUS]
| Times (e1, e2) -> compile e1 @ compile e2 @ [TIMES]
| And (e1, e2) -> compile e1 @ compile e2 @ [AND]
| Or (e1, e2) -> compile e1 @ compile e2 @ [OR]
| Not e1 -> compile e1 @ [NOT]
| Eq (e1, e2) -> compile e1 @ compile e2 @ [EQ]
| Gt (e1, e2) -> compile e1 @ compile e2 @ [GT];;
;;

let rec stackmc s c = match s, c with
s', LDN (n) :: c' -> stackmc ((Num n) :: s') c'
| s', LDB (b) :: c' -> stackmc ((B1 b) :: s') c'
| (Num n2) :: (Num n1) :: s', PLUS :: c' -> stackmc ((Num (n1 + n2)) :: s') c'
| (Num n2) :: (Num n1) :: s', TIMES :: c' -> stackmc ((Num (n1 * n2)) :: s') c'
| (B1 b2) :: (B1 b1) :: s', AND :: c' -> stackmc ((B1 (myAnd b2 b1)) :: s') c'
| (B1 b2) :: (B1 b1) :: s', OR :: c' -> stackmc ((B1 (myOr b2 b1)) :: s') c'
| (B1 b) :: s', NOT :: c' -> stackmc ((B1 (myNot b)) :: s') c'
| (Num n2) :: (Num n1) :: s', EQ :: c' -> stackmc ((B1 (bool2myBool (n1 = n2))) :: s') c'
| (Num n2) :: (Num n1) :: s', GT :: c' -> stackmc ((B1 (bool2myBool (n1 > n2))) :: s') c'
| a :: [], [] -> a
| _, _ -> raise Stuck;;





