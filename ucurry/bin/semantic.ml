open Ast 
module StringMap = Map.Make(String) 

exception TypeError of string 
type type_env = typ StringMap.t
type arg_type = typ 
type ret_type = typ 
type constructor_env = (arg_type * ret_type) StringMap.t 

let curry f a b = f (a, b)
let pair_with a = function b -> (b, a)
let o f g x = f (g x)
let bindAll (keys: string list) (vals: 'a list) (map : 'a StringMap.t)= List.fold_right2 StringMap.add  keys vals map
let getOp default = function
                  | Some a -> a 
                  | None -> default

let rec eqType = function 
                    (INT_TY, INT_TY) -> true
                  | (STRING_TY, STRING_TY) -> true
                  | (UNIT_TY, UNIT_TY) -> true   
                  | (BOOL_TY, BOOL_TY) -> true
                  | (LIST_TY tau1, LIST_TY tau2) -> eqType(tau1, tau2)
                  | (FUNCTION_TY (tau1, tau2), FUNCTION_TY(tau1', tau2')) -> eqType(tau1, tau1') && eqType(tau2, tau2')
                  | (CONSTRUCTOR_TY n1, CONSTRUCTOR_TY n2) -> String.equal n1 n2
                  | (TUPLE_TY tys1, TUPLE_TY tys2) -> List.for_all2 (curry eqType) tys1 tys2
                  | _ -> false 

let rec legalPattern (con_env : constructor_env)  (tau : typ)  (pat : pattern)= 
  match pat with
  | VAR_PAT _ | WILDCARD -> tau 
  | CON_PAT (name, None) -> 
    let (exp_tau, ret_tau) = StringMap.find name con_env 
    in if (eqType (tau, ret_tau) && eqType (exp_tau, UNIT_TY)) then ret_tau else raise (TypeError " invalid pattern matching")
  | CON_PAT (name, Some pats) -> 
    let (exp_tau, ret_tau) = StringMap.find name con_env 
    in if (eqType (tau, ret_tau)) then 
      (match exp_tau with
      | TUPLE_TY types -> 
        let  _ = List.map2 (legalPattern con_env) types pats 
        in ret_tau
      | _ -> raise (TypeError "illy formed type constructor"))
      else raise (TypeError "ill formed type ")
  
let eqTypes = List.for_all2 (curry eqType)       
let lit_ty  = function 
                  INT _ -> INT_TY
                | STRING _ -> STRING_TY 
                | BOOL _ -> BOOL_TY
                | UNIT -> UNIT_TY 
                (* TODO: work on the list and tuple rule *)
                | _ -> UNIT_TY

let rec typ_of (exp : Ast.expr) (ty_env : type_env) (con_env: constructor_env)  = 
  let rec ty = function 
                  Literal l -> lit_ty l 
                | Var x -> StringMap.find x ty_env
                | Assign (x, e) -> 
                  let var_ty = StringMap.find x ty_env 
                  in  if eqType(var_ty, ty e) 
                      then var_ty 
                      else raise (TypeError "assigned unmatched type")
                | Apply (e, es) -> 
                  (match (ty e, List.map ty es) with
                      | (FUNCTION_TY (tau1, tau2), types) -> 
                        let rec matchfun tau types = 
                          (match (tau, types) with
                          | (_, []) -> tau
                          | (FUNCTION_TY (tau1, tau2), hd :: tl) -> if eqType(tau1, hd) then matchfun tau2 tl else raise (TypeError "argument type not matched")
                          | _ -> raise (TypeError "apply non-function"))
                        in matchfun (FUNCTION_TY (tau1, tau2)) types
                      | (_, _)-> raise (TypeError "applied a non-function"))
                | If (e1, e2, e3) -> 
                  (match (ty e1, ty e2, ty e3) with
                  | (BOOL_TY, tau1, tau2) -> if eqType(tau1, tau2) then tau1 else raise (TypeError "if branches contain different types")
                  | _ -> raise (TypeError "if condition contains non-boolean types"))
                | Let (bindings, e) ->
                  let (vars, es) = List.split bindings
                  in  let (types, names) = List.split vars 
                  in  let newEnv =  bindAll names types ty_env 
                  in  let sameTypes = eqTypes types (List.map ty es)
                  in  if sameTypes then typ_of e newEnv con_env else raise (TypeError "binding types are not annotated correctly")
                | Begin([]) -> UNIT_TY
                | Begin(es) -> 
                  let types = List.map ty es in o List.hd List.rev types
                | Binop(e1, b, e2) ->
                  let tau1 = ty e1 
                  in let tau2 = ty e2 
                  in let same = eqType (tau1, tau2)
                  in (match b with
                      | Add | Sub| Mult| Div| Mod when same && eqType (tau1, INT_TY)-> INT_TY
                      | Geq | Less| Leq | Greater when same && eqType (tau1, INT_TY)-> BOOL_TY
                      | Add | Or when same && eqType (tau1, BOOL_TY) -> BOOL_TY
                      | Equal | Neq when same -> BOOL_TY
                      | Cons when eqType(LIST_TY tau1, tau2) -> LIST_TY tau1 
                      | _ -> raise (TypeError "type error in binary operation")) 
                | Unop(u, e2) -> 
                  let tau1 = ty e2 
                  in (match (u, tau1) with 
                      | (Neg, INT_TY) -> INT_TY
                      | (Not, BOOL_TY) -> BOOL_TY
                      | (Hd, LIST_TY tau1) -> tau1
                      | (Tl, LIST_TY tau1) -> LIST_TY tau1  
                      | _ -> raise (TypeError "type error in unoary operaion"))
                | Lambda (ty, formals, body) -> ty (* TODO: checking for lambda's type *) 
                | Construct (name, exp) -> 
                  let (exp_tau, ret_tau) = StringMap.find name con_env
                  in  if eqType(exp_tau, ty exp) 
                      then ret_tau 
                      else raise (TypeError "type error in construct")
                | Case (exp, cases) ->
                  let tau1 = ty exp 
                  in let (patterns, es) =  List.split cases 
                  in let _ = List.map (legalPattern con_env tau1) patterns 
                  in let taus =  List.map ty es 
                  in let (allSame, tau) = 
                    List.fold_left 
                        (fun (same, tau) tau' -> 
                             (same && eqType(tau, tau'), tau)) 
                        (true, List.hd taus) (List.tl taus) 
                  in 
                    if allSame then tau else raise (TypeError "ill typed case expression ")
                | Noexpr -> UNIT_TY
  in ty exp 

let type_def (def : def) (con_env: constructor_env) (ty_env: type_env) = 
  let ty = 
  function Function (tau, funname, args, body) ->
        let tau' = typ_of (Lambda (tau, args, body)) ty_env con_env 
        in if eqType(tau',tau) 
          then (StringMap.add funname tau ty_env, con_env)
          else raise (TypeError "invalid type in function definition")
      | Datatype (name, val_cons) ->
        let cons_taus = CONSTRUCTOR_TY name 
        in let (names, tausop) = List.split val_cons 
        in let taus = List.map (o (pair_with cons_taus) (getOp UNIT_TY)) tausop 
        in 
        (ty_env, bindAll names taus con_env)
      | Variable (tau, name, e) ->  
        if eqType (tau, typ_of e ty_env con_env) 
        then (StringMap.add name tau ty_env, con_env) 
        else raise (TypeError "type mismatch in variable definition")
      | Exp e ->  
        let _= typ_of e ty_env con_env in (ty_env, con_env)
  in 
      ty def 

let typecheck (defs : Ast.program) = 
  List.fold_left 
  (fun (ty_env, con_env) def -> type_def def con_env ty_env) 
  (StringMap.empty, StringMap.empty)
  defs 