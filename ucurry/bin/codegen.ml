(* module L = Llvm
module A = Ast
module C = Cast
open Last
module StringMap = Map.Make (String)

exception CODEGEN_NOT_YET_IMPLEMENTED of string
(* TODO: consider adding an SAST to add type to each expression *)

let build_main_body defs =
  let context = L.global_context () in
  let i32_t = L.i32_type context in
  let i8_t = L.i8_type context in
  let void_t = L.void_type context in
  let main_ftype = L.function_type void_t [| i32_t |] in
  let the_module = L.create_module context "uCurry" in

  let main_function = L.define_function "main" main_ftype the_module in
  let builder = L.builder_at_end context (L.entry_block main_function) in

  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
  and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder
  and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in
  let rec expr builder = function
    | C.Literal (INT i) -> L.const_int i32_t i (* TODO: integers only for now *)
    | C.Literal (STRING s) -> L.build_global_stringptr s "str" builder
    | C.Assign _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "Assign")
    | C.Apply _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "APPLY")
    | C.Unop (unop, e) -> (
        let printf_t : L.lltype =
          L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
        in
        let printf_func : L.llvalue =
          L.declare_function "printf" printf_t the_module
        in

        let e' = expr builder e in
        match (unop, e) with
        | A.Print, C.Literal (INT _) ->
            L.build_call printf_func [| int_format_str; e' |] "printf" builder
        | A.Print, C.Literal (STRING _) ->
            L.build_call printf_func
              [| string_format_str; e' |]
              "printf" builder
        | _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "unop"))
    | _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "catchall")
  in

  let rec stmt builder = function
    | C.Exp e ->
        let _ = expr builder e in
        builder
    | _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "catchall")
  in

  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
    | Some _ -> ()
    | None -> ignore (instr builder)
  in

  (* Build the main function body *)
  let main_builder = List.fold_left stmt builder defs in

  (* Add a void return to main *)
  let _ = add_terminal main_builder L.build_ret_void in

  the_module *)
