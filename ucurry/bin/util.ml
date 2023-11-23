module L = Llvm
module A = Ast
module C = Cast 
exception Impossible of string

let set_data_field value index struct_ptr builder =
  let field_ptr = L.build_struct_gep struct_ptr index "struct_field" builder in
  L.build_store value field_ptr builder

let get_data_field index struct_ptr builder name =
  let field_ptr = L.build_struct_gep struct_ptr index "struct_field" builder in
  L.build_load field_ptr name builder

let curry f a b = f (a, b)
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
