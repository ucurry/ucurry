(* module A = Ast *)
module A = Ast
module L = Last
module S = Set.Make (String)
module C = Cast

exception CLOSURE_NOT_YET_IMPLEMENTED of string 

(* do free variable analysis *)
let rec free ((t, exp) : L.sexpr) : S.t = 
  let unionSets sets = List.fold_left (fun acc s -> S.union s acc) S.empty sets in 
  let freeThunk thunk = free (t, L.Lambda thunk) in
match exp with 
  | L.Literal _ -> S.empty
  | L.Var name -> S.of_list [name]
  | L.Assign (_, thunk) -> freeThunk thunk
  | L.Apply (f, thunks) -> 
      let freeVars = unionSets (List.map freeThunk thunks) in
      S.union (free f) freeVars
  | L.If (s1, s2, s3) -> unionSets (List.map free [s1; s2; s3])
  | L.Let (bindings, body) -> 
      let locals, thunks = List.split (List.map (fun ((_, n), thunk) -> (n, thunk)) bindings) in
      let freeXSet = S.of_list locals in 
      let freeESet = unionSets (List.map freeThunk thunks) in 
      let freeBody = free body in
      S.union freeESet (S.diff freeBody freeXSet)
  | L.Begin sexprs -> unionSets (List.map free sexprs)
  | L.Binop (operand1, _, operand2) -> S.union (free operand1) (free operand2)
  | L.Unop (_, operand) -> free operand
  | L.Case (scrutinee, cexprs) -> 
      let fscrutinee = free scrutinee in 
      List.fold_left 
        (fun freevars (_, sexpr) -> S.union freevars (free sexpr)) 
      fscrutinee cexprs
  | L.Lambda (formals, sexpr) -> S.diff (free sexpr) (S.of_list formals)
  | _ -> S.empty

(* close expression *)
(* closeExp ... *)
let rec closeExpWith (captured : string list) ((ty, tope) : L.sexpr) : C.sexpr = 

  let rec closeExp (captured : string list) (exp : L.expr) : C.expr =
  
    let asClosure ((formals, (fty, body)) as lambda) : C.closure = 
      let freeVars = S.elements (free (A.UNIT_TY, L.Lambda lambda)) in 
      let captured' = List.map (fun x -> closeExpWith captured (L.Var x)) freeVars in 
      ((formals, closeExpWith freeVars body), captured')
      in

    match exp with
    | L.Literal l -> Literal l
    | L.Var name -> raise (CLOSURE_NOT_YET_IMPLEMENTED "var")
    | L.Assign _ -> raise (CLOSURE_NOT_YET_IMPLEMENTED "assign")
    | L.Apply _ ->  raise (CLOSURE_NOT_YET_IMPLEMENTED "placeholder")
    | L.If _ -> raise (CLOSURE_NOT_YET_IMPLEMENTED "placeholder")
    | L.Let _-> raise (CLOSURE_NOT_YET_IMPLEMENTED "placeholder")
    | L.Begin _ -> raise (CLOSURE_NOT_YET_IMPLEMENTED "placeholder")
    | L.Binop _ -> raise (CLOSURE_NOT_YET_IMPLEMENTED "placeholder")
    | L.Unop (op, e) -> Unop (op, closeExpWith e)
    | L.Case _ ->   raise (CLOSURE_NOT_YET_IMPLEMENTED "placeholder")
    | L.Lambda _ ->raise (CLOSURE_NOT_YET_IMPLEMENTED "placeholder")
    | _ -> raise (Failure "CloseExp Not implemented For Most Cases")
  in raise (CLOSURE_NOT_YET_IMPLEMENTED "siduhfsd")


(* close definition *)
let close (def : L.def) : Cast.def =
  match def with
  | L.Exp e -> C.Exp (closeExpWith [] e)
  | _ -> raise (Failure "Close Not implemented For Most Cases")

let closeProgram (p : L.program) : C.program = List.map close p
