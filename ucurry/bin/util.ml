module L = Llvm
module A = Ast
module C = Cast 
exception Impossible of string



(* type utility function *)
let list_subtype = function
  | Ast.LIST_TY tau -> tau
  | _ -> raise (Invalid_argument "not a list type")

(* utility functions for llvm *)
let set_data_field value index struct_ptr builder =
  let field_ptr = L.build_struct_gep struct_ptr index "struct_field" builder in
  L.build_store value field_ptr builder

let get_data_field index struct_ptr builder name =
  let field_ptr = L.build_struct_gep struct_ptr index "struct_field" builder in
  L.build_load field_ptr name builder

(* genral utilities funcion  *)
let curry f a b = f (a, b)
let fst (a, _) = a
let snd (_, b) = b
let mid (_, b, _) = b
let pair_with a b = (b, a)
let o f g x = f (g x)
let getOp default = function Some a -> a | None -> default

let rec fold_left_i (f : 'a -> int -> 'c -> 'c) (i : int) (acc : 'c)
    (l : 'a list) =
  match l with [] -> acc | x :: xs -> fold_left_i f (i + 1) (f x i acc) xs

let rec getFormalTypes = function
  | A.FUNCTION_TY (UNIT_TY, retty) -> getFormalTypes retty
  | A.FUNCTION_TY (formalty, retty) -> formalty :: getFormalTypes retty
  | _ -> []

let rec getRetType = function
  | A.FUNCTION_TY (_, retty) -> getRetType retty
  | retty -> retty

let getFuntionType ty = (getFormalTypes ty, getRetType ty)
let rec map_i (f : 'a -> int -> 'b) (i : int) (l : 'a list) =
  match l with [] -> [] | x :: xs -> f x i :: map_i f (i + 1) xs

let list_to_arr l = Array.init (List.length l) (List.nth l)
