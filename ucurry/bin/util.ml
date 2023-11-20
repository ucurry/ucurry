(* module L = LLVM *)

exception Impossible of string

(* let set_data_field value index struct_ptr builder name =
  let field_ptr = L.build_struct_gep struct_ptr index "struct_field" builder in
  L.build_store value field_ptr builder

let get_data_field index struct_ptr builder name =
  let field_ptr = L.build_struct_gep struct_ptr index "struct_field" builder in
  L.build_load field_ptr name builder *)

let curry f a b = f (a, b)
let pair_with a b = (b, a)
let o f g x = f (g x)
let getOp default = function Some a -> a | None -> default

let rec fold_left_i (f : 'a -> int -> 'c -> 'c) (i : int) (acc : 'c)
    (l : 'a list) =
  match l with [] -> acc | x :: xs -> fold_left_i f (i + 1) (f x i acc) xs

let getFunctiontype funty = (* TODO: for now the function type can only be one value!!  *)
    match funty with
    | Ast.FUNCTION_TY (formalty, retty) -> (formalty, retty)
    | _ -> raise (Impossible "not a function type")