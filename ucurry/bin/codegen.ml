module L = Llvm
module A = Ast
module S = Sast
module C = Cast
module StringMap = Map.Make (String)

exception CODEGEN_NOT_YET_IMPLEMENTED of string
exception SHOULDNT_RAISED of string
exception REACHED
(* TODO: consider adding an SAST to add type to each expression *)

let build_main_body defs =
  let context = L.global_context () in
  let i32_t = L.i32_type context in
  let i8_t = L.i8_type context in
  let i1_t = L.i1_type context in
  let void_t = L.void_type context in
  let string_t = L.pointer_type i8_t in

  let main_ftype = L.function_type void_t [| i32_t |] in
  let the_module = L.create_module context "uCurry" in
  let ltype_of_type = function
    | A.INT_TY -> i32_t
    | A.BOOL_TY -> i1_t
    | A.STRING_TY -> string_t
    | _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "ltype_of_type")
  in

  (* | A.LIST_TY t       ->
     | A.UNIT_TY         ->
     | A.FUNCTION_TY (t1, t2) ->
     | A.CONSTRUCTOR_TY s ->
     | A.TUPLE_TY taus    -> *)
  let main_function = L.define_function "main" main_ftype the_module in
  let builder = L.builder_at_end context (L.entry_block main_function) in

  let int_format_str = L.build_global_stringptr "%d" "fmt" builder
  and string_format_str = L.build_global_stringptr "%s" "fmt" builder
  and true_str = L.build_global_stringptr "true" "true_s" builder
  and false_str = L.build_global_stringptr "false" "false_s" builder
  and int_nl_format_str = L.build_global_stringptr "%d\n" "fmt" builder
  and string_nl_format_str = L.build_global_stringptr "%s\n" "fmt" builder
  and true_nl_str = L.build_global_stringptr "true\n" "true_s" builder
  and false_nl_str = L.build_global_stringptr "false\n" "false_s" builder in

  (* let string_pool = TODO for cast *)
  let lookup n varmap = StringMap.find n varmap in
  let getFunctiontype funty =
    match funty with
    | A.FUNCTION_TY (formalty, retty) -> (formalty, retty)
    | _ -> raise (SHOULDNT_RAISED "not a function type")
  in
  let deconstructSLambda (ty, se) =
    match (ty, se) with
    | A.FUNCTION_TY (formalty, retty), S.SLambda (formals, body) ->
        (formalty, retty, formals, body)
    | _ -> raise (SHOULDNT_RAISED "not an slambda type")
  in
  let getRetty funty =
    let _, retty = getFunctiontype funty in
    retty
  in

  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
    | Some _ -> ()
    | None -> ignore (instr builder)
  in

  let rec exprWithVarmap builder varmap =
    let rec expr builder (ty, top_exp) =
      match top_exp with
      |
      | _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "SIFN")
    in
    expr builder
  and generateFunction varmap name slambda =
    let formalty, retty, formals, body = deconstructSLambda slambda in
    let formaltypes = ltype_of_type formalty :: [] in
    let formalsandtypes = List.combine formaltypes formals in
    let ftype =
      L.function_type (ltype_of_type retty) (Array.of_list formaltypes)
    in
    let the_function = L.define_function name ftype the_module in
    let varmap' = StringMap.add name the_function varmap in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let add_formal m (t, n) p =
      let _ = L.set_value_name n p in
      let local = L.build_alloca t n builder in
      let _ = L.build_store p local builder in
      StringMap.add n local m (* return a new local varmap *)
    in
    let localvarmap =
      List.fold_left2 add_formal varmap formalsandtypes
        (Array.to_list (L.params the_function))
    in
    let e' = exprWithVarmap builder localvarmap body in
    let _ = add_terminal builder (fun b -> L.build_ret e' b) in
    (e', varmap')
  in

  (* varmap is the variable environment that maps (variable : string |---> to reg : llvale) *)
  let rec stmt builder varmap = function
    | S.SFunction (name, slambda) ->
        (* TODO: partial application?? does LLVM support that?? *)
        let _, varmap' = generateFunction varmap name slambda in
        (builder, varmap')
    | S.SVal (tau, name, e) ->
        (* Handle string -> create a global string pointer and assign the global name to the name *)
        let e' = exprWithVarmap builder varmap e in
        let reg = L.build_alloca (ltype_of_type tau) name builder in
        let varmap' = StringMap.add name reg varmap in
        let _ = L.build_store e' (lookup name varmap') builder in
        (builder, varmap')
    | S.SExp e ->
        let _ = exprWithVarmap builder varmap e in
        (builder, varmap)
    | _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "catchall")
  in

  (* Build the main function body *)
  let main_builder, _ =
    List.fold_left
      (fun (_, varmap) def -> stmt builder varmap def)
      (builder, StringMap.empty) defs
  in

  (* Add a void return to main *)
  let _ = add_terminal main_builder L.build_ret_void in

  the_module
