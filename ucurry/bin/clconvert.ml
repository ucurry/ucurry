(* module A = Ast *)
module L = Last
module S = Set.Make (String)
module C = Cast

exception NOT_YET_IMPLEMENTED of string 

(* do free variable analysis *)
let rec free (exp : L.expr) : S.t = match exp with 
  | L.Literal v -> S.empty
  | _ -> S.empty

(* close expression *)
(* closeExp ... *)
let rec closeExpWith ((ty, tope) : L.sexpr) : C.sexpr = 
  let rec closeExp (exp : L.expr) : C.expr =
    match exp with
    | Literal l -> Literal l
    | Unop (op, e) -> Unop (op, closeExpWith e)
    | _ -> raise (Failure "CloseExp Not implemented For Most Cases")
  in 
(ty, closeExp tope)


(* close definition *)
let close (def : L.def) : Cast.def =
  match def with
  | L.Exp e -> C.Exp (closeExpWith e)
  | _ -> raise (Failure "Close Not implemented For Most Cases")

let closeProgram (p : L.program) : C.program = List.map close p
