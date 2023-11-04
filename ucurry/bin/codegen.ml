module L = Llvm
module A = Ast
open Last

module StringMap = Map.Make(String)
exception NOT_YET_IMPLEMENTED
(* TODO: consider adding an SAST to add type to each expression *)

(* need a minimum closure implementation *)
let translate (defs) = 
  let context    = L.global_context () in
  let i32_t      = L.i32_type context 
  and the_module = L.create_module context "uCurry" in 

  let ltype_of_typ = function 
      (* A.STRING_TY -> i32_t *)
      A.INT_TY -> i32_t
    | _ -> raise NOT_YET_IMPLEMENTED
  in 

  let printf_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
     L.declare_function "printf" printf_t the_module in

  let printbig_t = L.function_type i32_t [| i32_t |] in
  let printbig_func = L.declare_function "printbig" printbig_t the_module in
  
  (* TODO: implement build_def *)
  

  let rec expr builder (exp : Last.expr) = match exp with 
    Literal v -> L.const_int i32_t i (* TODO: integers only for now *)
    | Unop (unop, e) -> 
      let e' = expr builder e in 
        (match unop with
            A.Print -> L.build_call printf_func [| int_format_str ; e'|] "printf" builder 
          | _ -> raise NOT_YET_IMPLEMENTED)
    | _ -> raise NOT_YET_IMPLEMENTED
  in 

  let rec stmt builder = function 
    Exp e -> let _ = expr builder e in builder 
    | _ -> raise NOT_YET_IMPLEMENTED
  
  let builder = List.fold_left (fun b acc -> stmt builder acc) fdecl.sbody defs in
  List.iter build_def defs;
  the_module
  
(* at least for a function like print hi *)
