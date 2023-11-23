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
    | C.Closure ((formals, body), cap) ->
        (getFormalTypes ty, getRetType ty, formals, body, cap)
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

  let get_struct_type typs = 
    let field_types = Array.of_list (List.map ltype_of_type typs) in 
    (L.struct_type context field_types)
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
      | C.Apply ((ft, f) as sf, args) ->
          let fretty = getRetType ft in
          let llargs = List.rev (List.map (expr builder clstruct) (List.rev args)) in
          let fdef = expr builder clstruct sf in 
          let result = match fretty with A.UNIT_TY -> "" | _ -> "apply" ^ "_result" in
          L.build_call fdef (Array.of_list llargs) result builder
      | C.If _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "if")
      | C.Let _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "let")
      | C.Begin _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "begin")
      | C.Binop _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "binop")
      | C.Unop (unop, inner_e) -> (
          let printf_t : L.lltype =
            L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
          in
          let printf_func : L.llvalue =
            L.declare_function "printf" printf_t the_module
          in
          let e' = expr builder clstruct inner_e in
          let tau, _ = inner_e in
          match (unop, tau) with
          | A.Print, A.INT_TY ->
              L.build_call printf_func [| int_format_str; e' |] "printf" builder
          | A.Print, A.STRING_TY ->
              L.build_call printf_func
                [| string_format_str; e' |]
                "printf" builder
          | A.Println, A.INT_TY ->
              L.build_call printf_func
                [| int_nl_format_str; e' |]
                "printf" builder
          | A.Println, A.STRING_TY ->
              L.build_call printf_func
                [| string_nl_format_str; e' |]
                "printf" builder
          | A.Println, A.BOOL_TY ->
              L.build_call printf_func
                [| int_nl_format_str; e' |]
                "printf" builder
          | A.Print, A.BOOL_TY ->
              L.build_call printf_func [| int_format_str; e' |] "printf" builder
          | A.Print, _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "print")
          | A.Println, _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "println")
          | A.Neg, _ -> L.build_neg e' "temp" builder
          | A.Not, _ -> L.build_not e' "temp" builder
          | A.Hd, _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "hd")
          | A.Tl, _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "tl"))
      | C.Captured index -> 
        U.get_data_field index clstruct builder "capvar"
      | C.Closure (_, _) ->
          let e', _ = generate_closure varmap "lambda" clstruct (ty, top_exp) in 
          e'
      | C.Case _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "case")
      | _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "SIFN")
    in
    expr builder clstruct 
  
  and generate_closure varmap name clstruct closure =
    let formaltypes, retty, formals, body, cap = deconstructClosure closure in

    (* Get the value of the captured list *)
    let captured_values = List.map (exprWithVarmap builder clstruct varmap) cap in

    (* get the type of captured list *)
    let captured_types = List.map (fun (t, _) -> t) cap in 

    (* Alloc for the struct *)
    let struct_ptr = L.build_alloca (get_struct_type captured_types) "captured_struct" builder in 

    (* set each struct field *)
    let _ = List.fold_left 
            (fun counter v -> let _ = Util.set_data_field v counter struct_ptr builder in counter + 1)
            0
            captured_values
    in 

    (* make the struct pointer global *)
    let struct_init = L.const_struct context (Array.of_list (List.map (fun t -> L.const_null t) (List.map ltype_of_type captured_types))) in
    let reg = L.define_global "captured" struct_init the_module in
    let e' = L.build_load struct_ptr "tmp" builder in
    let _ = L.build_store e' reg builder in 

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
    let e' = exprWithVarmap builder reg localvarmap body in
    let _ = add_terminal builder (fun b -> L.build_ret e' b) in
    (the_function, varmap')
  in

  (* varmap is the variable environment that maps (variable : string |---> to reg : llvale) *)
  let rec stmt builder varmap = function
    | C.Function (name, (ty, c)) -> 
        (* TODO: partial application?? does LLVM support that?? *)
        (* let _, varmap' = generate_closure varmap name (L.const_null (L.pointer_type void_t)) closure in
        (builder, varmap') *)
        stmt builder varmap (C.Val (ty, name, (ty, c)))
    | C.Val (tau, name, e) ->
        (* Handle string -> create a global string pointer and assign the global name to the name *)
        let e' = exprWithVarmap builder (L.const_null (L.pointer_type void_t) )varmap e in
        let reg = L.build_alloca (ltype_of_type tau) name builder in
        let varmap' = StringMap.add name reg varmap in
        let _ = L.build_store e' (lookup name varmap') builder in
        (builder, varmap')
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
