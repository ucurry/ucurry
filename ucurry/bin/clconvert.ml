(* module A = Ast *)
module A = Ast
module L = Last
module C = Cast

type freevar = A.typ * string
module FreeVar : Set.OrderedType with type t = freevar = struct 
  type t = freevar
  let compare (_, s1) (_, s2) = String.compare s1 s2
end
module S = Set.Make (FreeVar)

exception CLOSURE_NOT_YET_IMPLEMENTED of string 

(* do free variable analysis *)
let rec free ((t, exp) : L.sexpr) : S.t = 

  let unionSets (sets: S.t list) : S.t = 
        List.fold_left (fun acc s -> S.union s acc) S.empty sets in 
  let unionFree (sexprs: L.sexpr list) : S.t = 
        unionSets (List.map free sexprs) in

match exp with 
  | L.Literal _ -> S.empty
  | L.Var name -> S.of_list [(t, name)]
  | L.Assign (_, thunk) -> free thunk
  | L.Apply (f, thunks) -> unionFree (f::thunks)
  | L.If (s1, s2, s3) -> unionFree [s1; s2; s3]
  | L.Let (bindings, body) -> 
      let localsWithTypes, thunks = List.split bindings in 
      let freeXSet = S.of_list localsWithTypes in 
      let freeESet = unionFree thunks in
      let freeBody = free body in
      S.union freeESet (S.diff freeBody freeXSet)
  | L.Begin sexprs -> unionFree sexprs
  | L.Binop (operand1, _, operand2) -> S.union (free operand1) (free operand2)
  | L.Unop (_, operand) -> free operand
  | L.Case (scrutinee, cexprs) -> 
      let fscrutinee = free scrutinee in 
      List.fold_left 
        (fun freevars (_, sexpr) -> S.union freevars (free sexpr)) 
      fscrutinee cexprs
  | L.Lambda (formals, sexpr) -> 
      let formalType, _ = Util.getFunctiontype t in (* TODO: no auto curry yet; only one argument allowed *)
      let formalWithTypes = List.combine [formalType] formals in 
      S.diff (free sexpr) (S.of_list formalWithTypes)
  | _ -> S.empty

(* close expression *)
(* closeExp ... *)
let rec closeExpWith (captured : freevar list) ((ty, tope) : L.sexpr) : C.sexpr = 

  let rec closeExp (captured : freevar list) (exp : L.expr) : C.expr =

    let asClosure (funty: A.typ) (lambda: L.lambda) : C.closure = 
      let (formals, sbody) = lambda in
      let freeVarWithTypes = S.elements (free (funty, L.Lambda lambda)) in 
      let captured' = List.map (fun (t, n) -> closeExpWith captured (t, (L.Var n))) freeVarWithTypes in (* TODO: get the type of the free vars! *)
      ((formals, closeExpWith freeVarWithTypes sbody), captured')
    in

    match exp with
    | L.Literal l -> Literal l
    | L.Var name -> raise (CLOSURE_NOT_YET_IMPLEMENTED "var")
    | L.Assign _ -> raise (CLOSURE_NOT_YET_IMPLEMENTED "assign")
    | L.Apply _ ->  raise (CLOSURE_NOT_YET_IMPLEMENTED "placeholder")
    | L.If _ -> raise (CLOSURE_NOT_YET_IMPLEMENTED "placeholder")
    | L.Let _-> raise (CLOSURE_NOT_YET_IMPLEMENTED "placeholder")
    | L.Begin sexprs -> raise (CLOSURE_NOT_YET_IMPLEMENTED "placeholder")
    | L.Binop _ -> raise (CLOSURE_NOT_YET_IMPLEMENTED "placeholder")
    | L.Unop (op, se) -> Unop (op, closeExpWith [] se)
    | L.Case _ ->   raise (CLOSURE_NOT_YET_IMPLEMENTED "placeholder")
    | L.Lambda lambda -> C.Closure (asClosure ty lambda)
    | _ -> raise (Failure "CloseExp Not implemented For Most Cases")
  in raise (CLOSURE_NOT_YET_IMPLEMENTED "siduhfsd")


(* close definition *)
let close (def : L.def) : Cast.def =
  match def with
  | L.Exp e -> C.Exp (closeExpWith [] e)
  | _ -> raise (Failure "Close Not implemented For Most Cases")

let closeProgram (p : L.program) : C.program = List.map close p
