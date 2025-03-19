open Ast

(* Type environment to track variable types *)
type env = (string * typ) list

(* Type checking exceptions *)
exception Type_error of string

(* Main type checking function *)
let rec type_check (env:env) (e:expr) : typ * expr =
  match e with
  | BoolConst _ -> (TBool, e)
  | IntConst _ -> (TInt, e)
  | FloatConst _ -> (TFloat, e)
  | VectorConst(_, Some dim) -> (TVector(Some dim), e)
  | MatrixConst(_, Some rows, Some cols) -> (TMatrix(Some rows, Some cols), e)
  
  | Id name ->
      (try 
         (List.assoc name env, e)
       with Not_found -> 
         raise (Type_error ("Undefined variable: " ^ name)))
  
  | Binop(op, e1, e2) ->
      let (t1, e1') = type_check env e1 in
      let (t2, e2') = type_check env e2 in
      (match op, t1, t2 with
       | Add, TInt, TInt -> (TInt, Binop(op, e1', e2'))
       | Sub, TInt, TInt -> (TInt, Binop(op, e1', e2'))
       | Mult, TInt, TInt -> (TInt, Binop(op, e1', e2'))
       | Div, TInt, TInt -> (TInt, Binop(op, e1', e2'))
       | Mod, TInt, TInt -> (TInt, Binop(op, e1', e2'))
       
       | FAdd, TFloat, TFloat -> (TFloat, Binop(op, e1', e2'))
       | FSub, TFloat, TFloat -> (TFloat, Binop(op, e1', e2'))
       | FMult, TFloat, TFloat -> (TFloat, Binop(op, e1', e2'))
       | FDiv, TFloat, TFloat -> (TFloat, Binop(op, e1', e2'))
       
       | Eq, _, _ when t1 = t2 -> (TBool, Binop(op, e1', e2'))
       | Lt, TInt, TInt 
       | Gt, TInt, TInt 
       | Leq, TInt, TInt 
       | Geq, TInt, TInt 
       | Neq, TInt, TInt -> (TBool, Binop(op, e1', e2'))
       
       | And, TBool, TBool 
       | Or, TBool, TBool -> (TBool, Binop(op, e1', e2'))
       
       | VecAdd, TVector(Some d1), TVector(Some d2) when d1 = d2 ->
           (TVector(Some d1), Binop(op, e1', e2'))
       
       | VecDot, TVector(Some d1), TVector(Some d2) when d1 = d2 ->
           (TFloat, Binop(op, e1', e2'))
       
       | VecCross, TVector(Some 3), TVector(Some 3) ->
           (TVector(Some 3), Binop(op, e1', e2'))
       
       | MatAdd, TMatrix(Some r1, Some c1), TMatrix(Some r2, Some c2) 
         when r1 = r2 && c1 = c2 ->
           (TMatrix(Some r1, Some c1), Binop(op, e1', e2'))
       
       | MatMult, TMatrix(Some r1, Some c1), TMatrix(Some r2, Some c2) 
         when c1 = r2 ->
           (TMatrix(Some r1, Some c2), Binop(op, e1', e2'))
       
       | _ -> raise (Type_error "Type mismatch in binary operation"))
  
       | Unop(op, e1) ->
        let (t1, e1') = type_check env e1 in
        (match op, t1 with
         | Neg, TInt -> (TInt, Unop(op, e1'))
         | Not, TBool -> (TBool, Unop(op, e1'))
         | Abs, TInt -> (TInt, Unop(op, e1'))
         | FAbs, TFloat -> (TFloat, Unop(op, e1'))
         | VecMag, TVector _ -> (TFloat, Unop(op, e1'))
         | VecDim, TVector(Some _d) -> (TInt, Unop(op, e1'))
         | VecNorm, TVector _ -> (t1, Unop(op, e1'))
         | MatDet, TMatrix(Some r, Some c) when r = c -> 
             (TFloat, Unop(op, e1'))
         | MatTr, TMatrix(Some r, Some c) -> 
             (TMatrix(Some c, Some r), Unop(op, e1'))
         | _ -> raise (Type_error "Type mismatch in unary operation"))
    
    | VecScal(e1, e2) ->
        let (t1, e1') = type_check env e1 in
        let (t2, e2') = type_check env e2 in
        (match t1, t2 with
         | TFloat, TVector(Some d) -> (TVector(Some d), VecScal(e1', e2'))
         | TInt, TVector(Some d) -> (TVector(Some d), VecScal(e1', e2'))
         | _ -> raise (Type_error "Type mismatch in vector scalar multiplication"))
    
    | MatScal(e1, e2) ->
        let (t1, e1') = type_check env e1 in
        let (t2, e2') = type_check env e2 in
        (match t1, t2 with
         | TFloat, TMatrix(Some r, Some c) -> (TMatrix(Some r, Some c), MatScal(e1', e2'))
         | TInt, TMatrix(Some r, Some c) -> (TMatrix(Some r, Some c), MatScal(e1', e2'))
         | _ -> raise (Type_error "Type mismatch in matrix scalar multiplication"))
    
    | VecAngle(e1, e2) ->
        let (t1, e1') = type_check env e1 in
        let (t2, e2') = type_check env e2 in
        (match t1, t2 with
         | TVector(Some d1), TVector(Some d2) when d1 = d2 -> 
             (TFloat, VecAngle(e1', e2'))
         | _ -> raise (Type_error "Type mismatch in vector angle calculation"))
    
    | Assign(name, e1) ->
        let (t1, e1') = type_check env e1 in
        (match List.assoc_opt name env with
         | Some t when t = t1 -> (t1, Assign(name, e1'))
         | Some _ -> raise (Type_error ("Type mismatch in assignment to " ^ name))
         | None -> (t1, Assign(name, e1'))) (* New variable, will be added to env *)
    
    | If(e1, e2, e3) ->
        let (t1, e1') = type_check env e1 in
        let (t2, e2') = type_check env e2 in
        let (t3, e3') = type_check env e3 in
        if t1 <> TBool then
          raise (Type_error "Condition in if-statement must be boolean")
        else if t2 <> t3 then
          raise (Type_error "Types of then and else branches must match")
        else
          (t2, If(e1', e2', e3'))
    
    | For(var, e1, e2, body) ->
        let (t1, e1') = type_check env e1 in
        let (t2, e2') = type_check env e2 in
        if t1 <> TInt || t2 <> TInt then
          raise (Type_error "Loop bounds must be integers")
        else
          let env' = (var, TInt) :: env in
          let (_, body') = type_check env' body in
          (TInt, For(var, e1', e2', body'))
    
    | While(e1, body) ->
        let (t1, e1') = type_check env e1 in
        if t1 <> TBool then
          raise (Type_error "Condition in while loop must be boolean")
        else
          let (_, body') = type_check env body in
          (TBool, While(e1', body'))
    
    | Seq(exprs) ->
        let rec check_seq env = function
          | [] -> (TInt, []) (* Default type for empty sequence *)
          | [e] -> 
              let (t, e') = type_check env e in
              (t, [e'])
          | e::es ->
              let (_, e') = type_check env e in
              let (t, es') = check_seq env es in
              (t, e'::es')
        in
        let (t, exprs') = check_seq env exprs in
        (t, Seq(exprs'))
    
    | Input -> (TBool, e) (* Assuming Input returns success status *)
    | InputFile _ -> (TBool, e) (* Assuming InputFile returns success status *)
    | Print -> (TInt, e) (* Assuming Print returns number of items printed *)
    | PrintId _ -> (TInt, e) (* Assuming PrintId returns number of items printed *)
  
    | VectorConst(_, None) ->
        raise (Type_error "Vector dimension must be specified")
    | MatrixConst(_, None, _) | MatrixConst(_, _, None) ->
        raise (Type_error "Matrix dimensions must be specified")

