
(* module A = Ast *)
module L = Last

open Cast

module S = Set.Make(String)

(* do free variable analysis *)
let rec free (exp : L.expr): S.t = match exp with _ -> S.empty

(* close expression *)
(* closeExp ... *)
let rec closeExp (exp: L.expr): Cast.expr = match exp with 
    Literal l -> Literal l
  | Unop (op, e) -> Unop (op, closeExp e)
  | _ -> raise (Failure "CloseExp Not implemented For Most Cases")

(* close definition *)
let close (def: L.def): Cast.def = match def with
  Exp e -> Exp (closeExp e)
  | _ -> raise (Failure "Close Not implemented For Most Cases")


let closeProgram (p: L.program): Cast.program = List.map close p
