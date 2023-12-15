(* Utility functions for semantic analysis *)
(* Authors: Stephanie Xu, Vivian Li, Matt Zhou *)

open Typing
module A = Ast
module S = Sast
module U = Util
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

exception TypeError of string

let empty_env = StringMap.empty
let empty_set = StringSet.empty

(* get the nth element from a list  *)
let nth (l : 'a list) (n : int) =
  try List.nth l n
  with Failure _ | Invalid_argument _ ->
    raise (TypeError "access out of the bound")

let get_ft = function
  | FUNCTION_TY (formalty, retty) -> (formalty, retty)
  | ty -> raise (TypeError (string_of_typ ty ^ " not function type"))

let findType (name : string) (env : 'a StringMap.t) =
  try StringMap.find name env
  with Not_found -> raise (TypeError ("name " ^ name ^ " unbound"))

let findFunctionType (name : string) (env : S.type_env) =
  match findType name env with
  | FUNCTION_TY (argty, retty) -> (argty, retty)
  | _ -> raise (TypeError ("name " ^ name ^ " is not a function type"))

let mustAvailable (env : 'a StringMap.t) (name : string) =
  match StringMap.find_opt name env with
  | Some _ -> raise (TypeError ("name " ^ name ^ " has been defined"))
  | None -> name

let bindUnique (name : string) (value : 'a) (env : 'a StringMap.t) =
  StringMap.add (mustAvailable env name) value env

let bindAll (keys : string list) (vals : 'a list) (map : 'a StringMap.t) =
  List.fold_right2 StringMap.add keys vals map

let bindAllUnique (keys : string list) (vals : 'a list) (map : 'a StringMap.t) =
  List.fold_right2 bindUnique keys vals map

let bindAllPairs (map : 'a StringMap.t) (pairs : (string * 'a) list) =
  let names, values = List.split pairs in
  bindAll names values map

let subtypeOfList tau =
  match tau with
  | LIST_TY tau1 -> tau1
  | _ -> raise (TypeError ("expected list type but got " ^ string_of_typ tau))

(* return the final types if tau1 and tau2 can be the same, else raise TypeError *)
let rec get_checked_types tau1 tau2 =
  match (tau1, tau2) with
  | INT_TY, INT_TY -> tau1
  | STRING_TY, STRING_TY -> tau1
  | UNIT_TY, UNIT_TY -> tau1
  | BOOL_TY, BOOL_TY -> tau1
  | LIST_TY tau1, LIST_TY tau2 ->
      let tau = get_checked_types tau1 tau2 in
      LIST_TY tau
  | FUNCTION_TY (arg_tau1, ret_tau1), FUNCTION_TY (arg_tau2, ret_tau2) ->
      let arg_tau = get_checked_types arg_tau1 arg_tau2
      and ret_tau = get_checked_types ret_tau1 ret_tau2 in
      FUNCTION_TY (arg_tau, ret_tau)
  | CONSTRUCTOR_TY n1, CONSTRUCTOR_TY n2 ->
      if String.equal n1 n2 then tau1
      else raise (TypeError "failed to check equal type")
  | TUPLE_TY tys1, TUPLE_TY tys2 ->
      let tys =
        try List.map2 get_checked_types tys1 tys2
        with Invalid_argument _ ->
          raise
            (TypeError
               ("failed to check equal type between " ^ string_of_typ tau1
              ^ " and " ^ string_of_typ tau2))
      in
      TUPLE_TY tys
  | ANY_TY, t -> t
  | t, ANY_TY -> t
  | tau1, tau2 ->
      raise
        (TypeError
           ("failed to check eqaul type between " ^ string_of_typ tau1 ^ " and "
          ^ string_of_typ tau2))

let rec eqType tau1 tau2 =
  match (tau1, tau2) with
  | INT_TY, INT_TY -> true
  | STRING_TY, STRING_TY -> true
  | UNIT_TY, UNIT_TY -> true
  | BOOL_TY, BOOL_TY -> true
  | LIST_TY UNIT_TY, LIST_TY _ -> true
  | LIST_TY _, LIST_TY UNIT_TY -> true
  | LIST_TY tau1, LIST_TY tau2 -> eqType tau1 tau2
  | FUNCTION_TY (tau1, tau2), FUNCTION_TY (tau1', tau2') ->
      eqType tau1 tau1' && eqType tau2 tau2'
  | CONSTRUCTOR_TY n1, CONSTRUCTOR_TY n2 -> String.equal n1 n2
  | TUPLE_TY tys1, TUPLE_TY tys2 -> List.for_all2 eqType tys1 tys2
  | ANY_TY, _ -> true
  | _, ANY_TY -> true
  | _ -> false

let add_let_type (s, e) env =
  match e with A.Lambda (t, _, _) -> StringMap.add s t env | _ -> env
