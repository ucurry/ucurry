module L = Llvm
module A = Ast
module C = Cast
module U = Util
module CGUtil = Cgutil
module StringMap = Map.Make (String)

exception CODEGEN_NOT_YET_IMPLEMENTED of string
exception SHOULDNT_RAISED of string
exception REACHED

let build_main_body defs =
  let context = L.global_context () in
  let i32_t = L.i32_type context in
  let i8_t = L.i8_type context in
  let i1_t = L.i1_type context in
  let void_t = L.void_type context in
  let main_ftype = L.function_type void_t [| i32_t |] in
  let the_module = L.create_module context "uCurry" in
  let deconstructClosure (ty, se) =
    (* NOTE: didn't check if ty is actually a funty  *)
    match se with
    | C.Closure ((formals, body), cap) ->
        (U.getFormalTypes ty, U.getRetType ty, formals, body, cap)
    | _ -> raise (SHOULDNT_RAISED "not an C.closure type")
  in
  let datatype_map = CGUtil.build_datatypes context the_module defs in
  let ltype_of_type = CGUtil.ltype_of_type datatype_map the_module context in
  let get_struct_type typs =
    let field_types = Array.of_list (List.map ltype_of_type typs) in
    L.struct_type context field_types
  in
  let main_function = L.define_function "main" main_ftype the_module in
  let builder = L.builder_at_end context (L.entry_block main_function) in
  let int_format_str = L.build_global_stringptr "%d" "fmt" builder
  and string_format_str = L.build_global_stringptr "%s" "fmt" builder
  and int_nl_format_str = L.build_global_stringptr "%d\n" "fmt" builder
  and string_nl_format_str = L.build_global_stringptr "%s\n" "fmt" builder
  and string_pool = CGUtil.build_string_pool defs builder in

  (* let string_pool = TODO for cast *)
  let lookup n varmap = StringMap.find n varmap in
  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
    | Some _ -> ()
    | None -> ignore (instr builder)
  in

  let rec exprWithVarmap builder clstruct varmap =
    let rec expr builder (ty, top_exp) =
      match top_exp with
      | C.Literal l ->
          CGUtil.build_literal builder datatype_map context the_module
            string_pool ty l
      | C.Var name -> L.build_load (lookup name varmap) name builder
      | C.Assign (name, e) ->
          let e' = expr builder e in
          let _ = L.build_store e' (lookup name varmap) builder in
          e'
      | C.Apply (((ft, _) as sf), args) ->
          let fretty = U.getRetType ft in
          let llargs = List.rev (List.map (expr builder) (List.rev args)) in
          let fdef = expr builder sf in
          let result =
            match fretty with A.UNIT_TY -> "" | _ -> "apply" ^ "_result"
          in
          L.build_call fdef (Array.of_list llargs) result builder
      | C.If (pred, then_expr, else_expr) ->
          (* 90% code referenced from https://releases.llvm.org/12.0.1/docs/tutorial/OCamlLangImpl5.html#llvm-ir-for-if-then-else *)
          (* emit expression for condition code *)
          let pred_res = expr builder pred in
          (* start a block *)
          let start_bb = L.insertion_block builder in
          (* [the_if_fun] will own the [start_bb] *)
          let the_if_fun = L.block_parent start_bb in
          (* start building [then_bb] *)
          let then_bb = L.append_block context "then" the_if_fun in
          L.position_at_end then_bb builder;
          (* start build code for then branch *)
          let then_val = expr builder then_expr in
          (* make sure getting an updated builder position for later use in phi function *)
          let new_then_bb = L.insertion_block builder in
          (* repeat the same above for else branch *)
          let else_bb = L.append_block context "else" the_if_fun in
          L.position_at_end else_bb builder;
          let else_val = expr builder else_expr in
          let new_else_bb = L.insertion_block builder in
          (* emit the merge block *)
          let merge_bb = L.append_block context "ifcon" the_if_fun in
          L.position_at_end merge_bb builder;
          (* create the phi node and set up the block/value pair for the phi *)
          let incoming = [ (then_val, new_then_bb); (else_val, new_else_bb) ] in
          let phi = L.build_phi incoming "iftmp" builder in
          (* return to the start block to add conditional branch *)
          L.position_at_end start_bb builder;
          ignore (L.build_cond_br pred_res then_bb else_bb builder);
          (* set up branch for then and else block to go to merge block at the end *)
          L.position_at_end new_then_bb builder;
          ignore (L.build_br merge_bb builder);
          L.position_at_end new_else_bb builder;
          ignore (L.build_br merge_bb builder);
          L.position_at_end merge_bb builder;
          phi
      | C.Let (bindings, exp) ->
          let varmap' =
            List.fold_left
              (fun vm ((tau, name), e) ->
                let e' = expr builder e in
                let reg = L.build_alloca (ltype_of_type tau) name builder in
                let vm' = StringMap.add name reg vm in
                let _ = L.build_store e' (lookup name vm') builder in
                vm')
              varmap bindings
          in
          exprWithVarmap builder clstruct varmap' exp
      | C.Begin sexprs ->
          List.fold_left
            (fun _ sexpr -> expr builder sexpr)
            (L.const_int i1_t 0) sexprs
      | C.Binop (e1, binop, e2) -> (
          let e1' = expr builder e1
          and e2' = expr builder e2
          and tau_e, _ = e1 in
          match binop with
          | A.Add -> L.build_add e1' e2' "temp" builder
          | A.Sub -> L.build_sub e1' e2' "temp" builder
          | A.Mult -> L.build_mul e1' e2' "temp" builder
          | A.Div -> L.build_sdiv e1' e2' "temp" builder
          | A.Mod -> L.build_srem e1' e2' "temp" builder
          | A.And -> L.build_and e1' e2' "temp" builder
          | A.Or -> L.build_or e1' e2' "temp" builder
          | A.Equal -> (
              match tau_e with
              | INT_TY | BOOL_TY ->
                  L.build_icmp L.Icmp.Eq e1' e2' "temp" builder
              | _ ->
                  raise
                    (CODEGEN_NOT_YET_IMPLEMENTED
                       "other equality type not implemented"))
          | A.Neq -> L.build_icmp L.Icmp.Ne e1' e2' "temp" builder
          | A.Less -> L.build_icmp L.Icmp.Slt e1' e2' "temp" builder
          | A.Leq -> L.build_icmp L.Icmp.Sle e1' e2' "temp" builder
          | A.Greater -> L.build_icmp L.Icmp.Sgt e1' e2' "temp" builder
          | A.Geq -> L.build_icmp L.Icmp.Sge e1' e2' "temp" builder
          | A.Cons -> raise (CODEGEN_NOT_YET_IMPLEMENTED "cons"))
      | C.Unop (unop, inner_e) -> (
          let printf_t : L.lltype =
            L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
          in
          let printf_func : L.llvalue =
            L.declare_function "printf" printf_t the_module
          in
          let e' = expr builder inner_e in
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
          | A.Hd, _ ->
              let list_ptr = expr builder inner_e in
              Util.get_data_field 0 list_ptr builder "hd_field"
          | A.Tl, _ ->
              let list_ptr = expr builder inner_e in
              Util.get_data_field 1 list_ptr builder "tl_field"
          | A.IsNull, _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "is null")
          | _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "unop"))
      | C.Captured index -> U.get_data_field index clstruct builder "capvar"
      | C.Closure (_, _) ->
          let e', _ =
            generate_closure builder varmap "lambda" clstruct (ty, top_exp)
          in
          e'
      | C.Case _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "case")
      | C.At (e, i) ->
          let tuple_ptr = expr builder e in
          Util.get_data_field i tuple_ptr builder "tuple field"
      | _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "SIFN")
    in
    expr builder
  and generate_closure builder varmap name clstruct closure =
    let formaltypes, retty, formals, body, cap = deconstructClosure closure in

    (* Get the value of the captured list *)
    let captured_values =
      List.map (exprWithVarmap builder clstruct varmap) cap
    in

    (* get the type of captured list *)
    let captured_types = List.map (fun (t, _) -> t) cap in

    (* Alloc for the captured struct *)
    let struct_ptr =
      L.build_alloca (get_struct_type captured_types) "captured_struct" builder
    in

    (* set each struct field *)
    let _ =
      List.fold_left
        (fun counter v ->
          let _ = Util.set_data_field v counter struct_ptr builder in
          counter + 1)
        0 captured_values
    in

    (* make the struct pointer global *)
    let struct_init =
      L.const_struct context
        (Array.of_list
           (List.map
              (fun t -> L.const_null t)
              (List.map ltype_of_type captured_types)))
    in
    let reg = L.define_global "captured" struct_init the_module in
    let e' = L.build_load struct_ptr "tmp" builder in
    let _ = L.build_store e' reg builder in

    (* build the function *)
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
      StringMap.add n local m (* return a new local varmap *)
    in
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
        let e' =
          exprWithVarmap builder (L.const_null (L.pointer_type void_t)) varmap e
        in
        let reg = L.build_alloca (ltype_of_type tau) name builder in
        let varmap' = StringMap.add name reg varmap in
        let _ = L.build_store e' (lookup name varmap') builder in
        (builder, varmap')
    | C.Exp e ->
        let _ =
          exprWithVarmap builder (L.const_null (L.pointer_type void_t)) varmap e
        in
        (builder, varmap)
    | C.Datatype _ -> (builder, varmap)
    | C.CheckTypeError _ ->
        failwith "codegen not implemented for checktype error"
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
