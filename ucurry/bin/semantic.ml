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
let getOp default = function 
                  | Some a -> a 
                  | None -> default
let findType (name: string) (env : 'a StringMap.t) = 
  try StringMap.find name env 
  with Not_found -> raise (TypeError ("name " ^ name ^ "unbound"))

let mustAvailable  (env: 'a StringMap.t) (name: string) = 
  (match StringMap.find_opt name env with
      | Some _ -> raise (TypeError ("name " ^ name ^ " has been defined"))
      | None -> name)

let bindUnique  (name: string) (value: 'a) (env: 'a StringMap.t)= 
  StringMap.add (mustAvailable env name) value env

  let bindAll (keys: string list) (vals: 'a list) (map : 'a StringMap.t) = 
    List.fold_right2 StringMap.add  keys vals map

let bindAllUnique (keys: string list) (vals: 'a list) (map : 'a StringMap.t) = 
    List.fold_right2 bindUnique keys vals map

let bindAllPairs (map : 'a StringMap.t) (pairs: (string * 'a) list) = 
    let (names, values) = List.split pairs in bindAll names values map 
      
let rec eqType = function 
                    (INT_TY, INT_TY) -> true
                  | (STRING_TY, STRING_TY) -> true
                  | (UNIT_TY, UNIT_TY) -> true   
                  | (BOOL_TY, BOOL_TY) -> true
                  | (LIST_TY UNIT_TY, LIST_TY _) -> true (* TODO: HACK all empty list is of type LIST UNIT, which can be matched to any LIST TYPE  *)
                  | (LIST_TY _, LIST_TY UNIT_TY) -> true
                  | (LIST_TY tau1, LIST_TY tau2) -> eqType(tau1, tau2)
                  | (FUNCTION_TY (tau1, tau2), FUNCTION_TY(tau1', tau2')) -> eqType(tau1, tau1') && eqType(tau2, tau2')
                  | (CONSTRUCTOR_TY n1, CONSTRUCTOR_TY n2) -> String.equal n1 n2
                  | (TUPLE_TY tys1, TUPLE_TY tys2) -> List.for_all2 (curry eqType) tys1 tys2
                  | _ -> false 

let rec legalPattern (con_env : constructor_env) (tau : typ)  (pat : pattern) = 
  match pat with
  | VAR_PAT x -> [(x, tau)]
  | WILDCARD -> []
  | CON_PAT (name, None) -> 
    let (exp_tau, ret_tau) = findType name con_env 
    in if (eqType (tau, ret_tau) && eqType (exp_tau, UNIT_TY)) then [] else raise (TypeError ("invalid pattern matching for pattern " ^ string_of_pattern pat ^ " expected " ^ string_of_typ tau ^ " get " ^ string_of_typ ret_tau ))
  | CON_PAT (name, Some pats) -> 
    let (exp_tau, ret_tau) = findType name con_env 
    in if (eqType (tau, ret_tau)) then 
      (match (exp_tau, pats) with
      | (TUPLE_TY types, _) -> List.concat (List.map2 (legalPattern con_env) types pats)
      | (tau, [pat]) ->  legalPattern con_env tau pat 
      | _ -> raise (TypeError "illy formed type constructor"))
      else raise (TypeError "ill formed type ")
  
let eqTypes = List.for_all2 (curry eqType)       

let rec typ_of (con_env: constructor_env) (ty_env : type_env) (exp : Ast.expr) = 
  let rec ty = function 
                  Literal l -> 
                  let rec lit_ty  = function 
                    INT _ -> INT_TY
                  | STRING _ -> STRING_TY 
                  | BOOL _ -> BOOL_TY
                  | UNIT -> UNIT_TY 
                  | LIST ([]) -> LIST_TY UNIT_TY (* TODO: decide on the type for empty list, or whether we allow for type variable *)
                  | LIST (hd :: tl) -> 
                    (match (lit_ty hd, lit_ty (LIST tl)) with
                  | (tau1, LIST_TY UNIT_TY) -> LIST_TY tau1 
                  | (tau1, LIST_TY tau2) -> if eqType (tau1, tau2) 
                                            then LIST_TY tau2 
                                            else raise (TypeError "list contain elements of different types")
                  | _ -> raise (TypeError "cons called on non-list"))
                  | TUPLE lits -> TUPLE_TY (List.map lit_ty lits)
                  | INF_LIST _ -> LIST_TY INT_TY
                  | Construct (name, exp) -> 
                    let (exp_tau, ret_tau) = findType name con_env
                    in  if eqType(exp_tau, ty exp) 
                        then ret_tau 
                        else raise (TypeError "type error in construct")
                    in  
                      lit_ty l 
                | Var x -> findType x ty_env 
                | Assign (x, e) -> 
                  let var_ty = findType x ty_env 
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
                  let (vars, es) = List.split bindings in
                  let (types, names) = List.split vars in
                  let newEnv =  bindAll names types ty_env in
                  let sameTypes = eqTypes types (List.map ty es) in
                  if sameTypes then typ_of con_env newEnv e else raise (TypeError "binding types are not annotated correctly")
                | Begin([]) -> UNIT_TY
                | Begin(es) -> 
                  let types = List.map ty es in o List.hd List.rev types
                | Binop(e1, b, e2) as exp ->
                  let tau1 = ty e1 in
                  let tau2 = ty e2 in
                  let same = eqType (tau1, tau2) in 
                     (match b with
                      | Add | Sub| Mult| Div| Mod when same && eqType (tau1, INT_TY)-> INT_TY
                      | Geq | Less| Leq | Greater when same && eqType (tau1, INT_TY)-> BOOL_TY
                      | And | Or when same && eqType (tau1, BOOL_TY) -> BOOL_TY
                      | Equal | Neq when same -> BOOL_TY
                      | Cons when eqType (LIST_TY tau1, tau2) -> LIST_TY tau1 
                      | _ -> raise (TypeError ("type error in expression " ^ (string_of_expr exp)))) 
                | Unop(u, e2) -> 
                  let tau1 = ty e2 
                  in (match (u, tau1) with 
                      | (Neg, INT_TY) -> INT_TY
                      | (Not, BOOL_TY) -> BOOL_TY
                      | (Hd, LIST_TY tau1) -> tau1
                      | (Tl, LIST_TY tau1) -> LIST_TY tau1  
                      | _ -> raise (TypeError "type error in unoary operaion"))
                | Lambda (ty, formals, body) ->
                    let rec check_lambda tau formals env = (match 
                      (tau, formals) with
                    | (_, []) -> if eqType (tau, typ_of con_env env body) 
                                 then ty else raise (TypeError "lambda type unmatch")
                    | (FUNCTION_TY (tau1, tau2), hd :: tl) -> check_lambda tau2 tl (StringMap.add hd tau1 env)
                    | _ -> raise(TypeError "lambda type unmatch"))
                    in  check_lambda ty formals ty_env 
                | Case (exp, cases) ->
                  let scrutinee = ty exp in 
                  let (patterns, es) =  List.split cases  in 
                  let bindings = List.map (legalPattern con_env scrutinee) patterns in 
                  let newEnvs = List.map (bindAllPairs ty_env) bindings in 
                  let taus = List.map2 (typ_of con_env) newEnvs es in 
                  let (allSame, tau) = 
                    List.fold_left 
                        (fun (same, tau) tau' -> (same && eqType(tau, tau'), tau)) 
                        (true, List.hd taus) (List.tl taus) 
                  in 
                    if allSame then tau else raise (TypeError "ill typed case expression ")
                | Noexpr -> UNIT_TY
  in ty exp 

let rec type_def (def : def) (ty_env: type_env) (con_env: constructor_env) = 
  let ty = 
  function Function (tau, funname, args, body) ->
        let tau' = typ_of con_env 
                          (bindUnique funname tau ty_env) 
                          (Lambda (tau, args, body)) 
        in if eqType(tau',tau) 
          then (bindUnique funname tau ty_env, con_env)
          else raise (TypeError "invalid type in function definition")
      | Datatype (name, val_cons) ->
        let cons_taus = CONSTRUCTOR_TY name 
        in let (names, tausop) = List.split val_cons 
        in let taus = List.map (o (pair_with cons_taus) (getOp UNIT_TY)) tausop 
        in   
        (ty_env, bindAllUnique names taus con_env)
      | Variable (tau, name, e) ->  
        if eqType (tau, typ_of con_env ty_env e)
        then (bindUnique name tau ty_env, con_env) 
        else raise (TypeError ("type mismatch in variable definition " ^ name))
      | Exp e ->  
        let _= typ_of con_env ty_env e in (ty_env, con_env)
      | CheckTypeError e -> 
          (try let _ = type_def e ty_env con_env 
              in  raise (TypeError "supposed to raise type error")
            with TypeError _ ->(ty_env, con_env))
        in 
        ty def 

let typecheck (defs : Ast.program) = 
  List.fold_left 
  (fun (ty_env, con_env) def -> type_def def  ty_env con_env) 
  (StringMap.empty, StringMap.empty)
  defs 


  (* TODO:
  1. variable redefinition policy --> does not allow 
  2. function type - do we support partial application?
  3. should value construct only takes in value or expression 
  4. revisit list pattern matching and how list value is represented in ast 
  5. decide on the type for empty list, or whether we allow for type variable 
  *)