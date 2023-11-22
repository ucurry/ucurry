module L = Llvm
module A = Ast
module C = Cast
module U = Util
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
  let rec getFormalTypes = function
    | A.FUNCTION_TY (A.UNIT_TY, _) -> []
    | A.FUNCTION_TY (formalty, retty) -> formalty :: getFormalTypes retty
    | _ -> []
  in
  let rec getRetType = function
    | A.FUNCTION_TY (_, retty) -> getRetType retty
    | retty -> retty
  in
  let deconstructClosure (ty, se) =
    (* NOTE: didn't check if ty is actually a funty  *)
    match se with
    | C.Closure ((formals, body), _) ->
        (getFormalTypes ty, getRetType ty, formals, body)
    | _ -> raise (SHOULDNT_RAISED "not an C.closure type")
  in
  let rec ltype_of_type = function
    | A.INT_TY -> i32_t
    | A.BOOL_TY -> i1_t
    | A.STRING_TY -> string_t
    | A.UNIT_TY -> void_t
    | A.FUNCTION_TY (_, retty) as ft -> 
        let formal_types = getFormalTypes ft in 
        let formal_lltypes = List.map ltype_of_type formal_types in 
        let fun_type = L.function_type (ltype_of_type retty) (Array.of_list formal_lltypes) in 
        L.pointer_type fun_type 
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
  and int_nl_format_str = L.build_global_stringptr "%d\n" "fmt" builder
  and string_nl_format_str = L.build_global_stringptr "%s\n" "fmt" builder in 

  (* let string_pool = TODO for cast *)
  let lookup n varmap = StringMap.find n varmap in
  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
    | Some _ -> ()
    | None -> ignore (instr builder)
  in 

  

  let rec exprWithVarmap builder clstruct varmap =
    let rec expr builder clstruct (ty, top_exp) =
      match top_exp with
      | C.Literal (INT i) -> L.const_int i32_t i
      | C.Literal (STRING s) -> L.build_global_stringptr s "strlit" builder
      | C.Literal (BOOL b) -> L.const_int i1_t (if b then 1 else 0)
      | C.Literal UNIT -> L.const_int i1_t 0
      | C.Var name -> L.build_load (lookup name varmap) name builder 
      | C.Assign (name, e) ->
          let e' = expr builder clstruct e in
          let _ = L.build_store e' (lookup name varmap) builder in
          e'
      | C.Apply ((ft, f), args) ->
          let fretty = getRetType ft in
          let llargs = List.rev (List.map (expr builder clstruct) (List.rev args)) in
          (match f with
          | C.Var fname ->
              let fdef = StringMap.find fname varmap in
              let result =
                match fretty with A.UNIT_TY -> "" | _ -> fname ^ "_result"
              in
              L.build_call fdef (Array.of_list llargs) result builder
          | C.Closure _ -> 
              let e' = expr builder clstruct (ft, f) in
              let result =
                match fretty with A.UNIT_TY -> "" | _ -> "lambda" ^ "_result"
              in
              L.build_call e' (Array.of_list llargs) result builder
          | C.Literal _-> failwith "literal in function application"
          | C.Captured i -> U.get_data_field i clstruct builder "capvar"
          | C.Apply ((innerft, C.Var innerfname), _) -> (* TODO: this is hard-coding for simple.uc *)
              let innerfretty = getRetType innerft in 
              let fdef = StringMap.find innerfname varmap in
              let result =
                match innerfretty with A.UNIT_TY -> "" | _ -> innerfname ^ "_result"
              in
              L.build_call fdef (Array.of_list llargs) result builder
          | _    -> raise (SHOULDNT_RAISED "Illegal function application"))
          (* | C.Apply ((innerft, innerf), innerargs) as innerapply ->
              let fdef = expr builder clstruct (ft, innerapply) in 
              let result =
                match fretty with A.UNIT_TY -> "" | _ -> "innerf" ^ "_result"
              in
              L.build_call fdef (Array.of_list llargs) result builder *)

      | C.If _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "if")
      | C.Let _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "let")
      | C.Begin _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "begin")
      | C.Binop _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "binop")
      | C.Unop _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "unop")
      | C.Captured index -> U.get_data_field index clstruct builder "capvar"
      | C.Closure (_, cap) ->
          let captured_values =
            Array.of_list (List.map (expr builder clstruct) cap)
          in
          let newcl_struct = L.const_struct context captured_values in
          let globalcl_struct = L.define_global "captured" newcl_struct the_module in 
          let e', _ = generate_lambda varmap "lambda" globalcl_struct (ty, top_exp) in 
          e'
      | C.Case _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "case")
      | _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "SIFN")
    in
    expr builder clstruct 
  and generate_lambda varmap name clstruct closure =
    let formaltypes, retty, formals, body = deconstructClosure closure in
    let formal_lltypes = List.map ltype_of_type formaltypes in
    let formalsandtypes = List.combine formal_lltypes formals in
    let ftype =
      L.function_type (ltype_of_type retty) (Array.of_list formal_lltypes)
    in
    let the_function = L.define_function name ftype the_module in
    let varmap' = StringMap.add name the_function varmap in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let add_formal m (t, n) p =
      let _ = L.set_value_name n p in
      let local = L.build_alloca t n builder in
      let _ = L.build_store p local builder in
      StringMap.add n local m (* return a new local varmap *)in
    let localvarmap =
      List.fold_left2 add_formal varmap formalsandtypes
        (Array.to_list (L.params the_function))
    in
    let e' = exprWithVarmap builder clstruct localvarmap body in
    let _ = add_terminal builder (fun b -> L.build_ret e' b) in
    (the_function, varmap')
  in

  (* varmap is the variable environment that maps (variable : string |---> to reg : llvale) *)
  let rec stmt builder varmap = function
    | C.Function (name, closure) -> 
        (* TODO: partial application?? does LLVM support that?? *)
        let _, varmap' = generate_lambda varmap name (L.const_null (L.pointer_type void_t)) closure in
        (builder, varmap')
    (* | S.SVal (tau, name, e) ->
        (* Handle string -> create a global string pointer and assign the global name to the name *)
        let e' = exprWithVarmap builder varmap e in
        let reg = L.build_alloca (ltype_of_type tau) name builder in
        let varmap' = StringMap.add name reg varmap in
        let _ = L.build_store e' (lookup name varmap') builder in
        (builder, varmap') *)
    | C.Exp e ->
        let _ = exprWithVarmap builder (L.const_null (L.pointer_type void_t)) varmap e in
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
