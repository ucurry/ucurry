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
  | A.FUNCTION_TY (formalty, retty) -> (formalty, retty)
  | ty -> raise (TypeError (A.string_of_typ ty ^ " not function type"))

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
  | A.LIST_TY tau1 -> tau1
  | _ -> raise (TypeError ("expected list type but got " ^ A.string_of_typ tau))

(* return the final types if tau1 and tau2 can be the same, else raise TypeError *)
let rec get_checked_types tau1 tau2 =
  match (tau1, tau2) with
  | A.INT_TY, A.INT_TY -> tau1
  | A.STRING_TY, A.STRING_TY -> tau1
  | A.UNIT_TY, A.UNIT_TY -> tau1
  | A.BOOL_TY, A.BOOL_TY -> tau1
  (* | A.LIST_TY A.UNIT_TY, A.LIST_TY _ -> tau2
  | A.LIST_TY _, A.LIST_TY A.UNIT_TY -> tau1 *)
  | A.LIST_TY tau1, A.LIST_TY tau2 ->
      let tau = get_checked_types tau1 tau2 in
      A.LIST_TY tau
  | A.FUNCTION_TY (arg_tau1, ret_tau1), A.FUNCTION_TY (arg_tau2, ret_tau2) ->
      let arg_tau = get_checked_types arg_tau1 arg_tau2
      and ret_tau = get_checked_types ret_tau1 ret_tau2 in
      A.FUNCTION_TY (arg_tau, ret_tau)
  | A.CONSTRUCTOR_TY n1, A.CONSTRUCTOR_TY n2 ->
      if String.equal n1 n2 then tau1
      else raise (TypeError "failed to check equal type")
  | A.TUPLE_TY tys1, A.TUPLE_TY tys2 ->
      let tys =
        try List.map2 get_checked_types tys1 tys2
        with Invalid_argument _ ->
          raise
            (TypeError
               ("failed to check equal type between " ^ A.string_of_typ tau1
              ^ " and " ^ A.string_of_typ tau2))
      in
      A.TUPLE_TY tys
  | tau1, tau2 ->
      raise
        (TypeError
           ("failed to check eqaul type between " ^ A.string_of_typ tau1
          ^ " and " ^ A.string_of_typ tau2))

let rec eqType tau1 tau2 =
  match (tau1, tau2) with
  | A.INT_TY, A.INT_TY -> true
  | A.STRING_TY, A.STRING_TY -> true
  | A.UNIT_TY, A.UNIT_TY -> true
  | A.BOOL_TY, A.BOOL_TY -> true
  | A.LIST_TY A.UNIT_TY, A.LIST_TY _ -> true
  | A.LIST_TY _, A.LIST_TY A.UNIT_TY -> true
  | A.LIST_TY tau1, A.LIST_TY tau2 -> eqType tau1 tau2
  | A.FUNCTION_TY (tau1, tau2), FUNCTION_TY (tau1', tau2') ->
      eqType tau1 tau1' && eqType tau2 tau2'
  | A.CONSTRUCTOR_TY n1, A.CONSTRUCTOR_TY n2 -> String.equal n1 n2
  | A.TUPLE_TY tys1, A.TUPLE_TY tys2 -> List.for_all2 eqType tys1 tys2
  | _ -> false
