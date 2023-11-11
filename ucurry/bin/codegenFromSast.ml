module L = Llvm
module A = Ast
module S = Sast
module StringMap = Map.Make (String)

exception CODEGEN_NOT_YET_IMPLEMENTED of string
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
  and int_nl_format_str = L.build_global_stringptr "%d\n" "fmt" builder
  and string_nl_format_str = L.build_global_stringptr "%s\n" "fmt" builder
  in 
  (* let string_pool = StringMap.empty in TODO *)
  let lookup n varmap = StringMap.find n varmap in

  let rec exprWithVarmap builder varmap =
    (* TODO: delete *)
    let rec expr builder (ty, top_exp) =
      match top_exp with
      | S.SLiteral (Construct (name, value)) ->
          raise (CODEGEN_NOT_YET_IMPLEMENTED "constructor literal")
      | S.SLiteral (INT i) -> L.const_int i32_t i
      | S.SLiteral (STRING s) -> L.build_global_stringptr s "str" builder
      | S.SLiteral (BOOL b) -> L.const_int i1_t (if b then 1 else 0)
      | S.SLiteral (LIST values) ->
          raise (CODEGEN_NOT_YET_IMPLEMENTED "list literal")
      | S.SLiteral (TUPLE values) ->
          raise (CODEGEN_NOT_YET_IMPLEMENTED "tuple literal")
      | S.SLiteral (INF_LIST i) ->
          raise (CODEGEN_NOT_YET_IMPLEMENTED "inf list")
      | S.SLiteral UNIT ->
          L.const_null void_t (* TOOD: double check unit value *)
      | S.SVar name ->
          L.build_load (lookup name varmap) name
            builder (* %a1 = load i32, i32* %a, align 4 *)
      | S.SAssign (name, e) ->
          let e' = expr builder e in
          let _ = L.build_store e' (lookup name varmap) builder in
          e'
      | S.SApply _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "APPLY")
      | S.SIf (pred, then_expr, else_expr) ->
          raise (CODEGEN_NOT_YET_IMPLEMENTED "APPLY")
      | S.SLet (bindings, exp) ->
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
          exprWithVarmap builder varmap' exp
      | S.SBegin _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "Begin")
      | S.SBinop (e1, binop, e2) -> (
          let e1' = expr builder e1 and e2' = expr builder e2 in
          match binop with
          | A.Add -> L.build_add e1' e2' "temp" builder
          | A.Sub -> L.build_sub e1' e2' "temp" builder
          | A.Mult -> L.build_mul e1' e2' "temp" builder
          | A.Div -> L.build_sdiv e1' e2' "temp" builder
          | A.Mod -> L.build_srem e1' e2' "temp" builder
          | A.And -> L.build_and e1' e2' "temp" builder
          | A.Or -> L.build_or e1' e2' "temp" builder
          | A.Equal -> L.build_icmp L.Icmp.Eq e1' e2' "temp" builder
          | A.Neq -> L.build_icmp L.Icmp.Ne e1' e2' "temp" builder
          | A.Less -> L.build_icmp L.Icmp.Slt e1' e2' "temp" builder
          | A.Leq -> L.build_icmp L.Icmp.Sle e1' e2' "temp" builder
          | A.Greater -> L.build_icmp L.Icmp.Sgt e1' e2' "temp" builder
          | A.Geq -> L.build_icmp L.Icmp.Sge e1' e2' "temp" builder
          | A.Cons -> raise (CODEGEN_NOT_YET_IMPLEMENTED "cons"))
      | S.SUnop (unop, inner_e) -> (
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
              L.build_call printf_func [| string_format_str; e' |] "printf" builder
          | A.Println, A.INT_TY ->
              L.build_call printf_func [| int_nl_format_str; e' |] "printf" builder
          | A.Println, A.STRING_TY ->
              L.build_call printf_func [| string_nl_format_str; e' |] "printf" builder
          | A.Println, A.BOOL_TY ->
              L.build_call printf_func [| int_nl_format_str; e' |] "printf" builder (* TODO: print "true" or "false" for bool? *)
          | A.Print, _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "A")
          | A.Println, _ ->raise (CODEGEN_NOT_YET_IMPLEMENTED "A")
          | A.Neg, _ -> L.build_neg e' "temp" builder
          | A.Not, _ -> L.build_not e' "temp" builder 
          | A.Hd, _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "A")
          | A.Tl, _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "A")
          )
      | S.SLambda _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "Lambda")
      | S.SCase _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "Case")
      | S.SNoexpr -> L.const_null void_t (* TOOD: double check noexpr value *)
      | _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "catchall")
    in
    expr builder
  in 

  (* varmap is the variable environment that maps variable name to its runtime location *)
  let rec stmt builder varmap = function
    (* | S.SFunction (funty, name, formals, body) -> raise (NOT-CODEGEN_NOT_YET_IMPLEMENTED "function") TODO: partial application?? does LLVM support that?? *)
        (* let (formalty, retty) = funty in 
        let formaltypes = Array.of_list [ltype_of_type formalty]
        let ftype = L.function_type (ltype_of_type retty) formaltypes in 
        let varmap' = StringMap.add name (L.define_function name ftype the_module) varmap in  *)
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

  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
    | Some _ -> ()
    | None -> ignore (instr builder)
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
