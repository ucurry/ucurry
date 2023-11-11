module L = Llvm
module A = Ast
module S = Sast
module StringMap = Map.Make (String)

exception CODEGEN_NOT_YET_IMPLEMENTED of string
(* TODO: consider adding an SAST to add type to each expression *)

let build_main_body defs =
  let context = L.global_context () in
  let i32_t   = L.i32_type context in
  let i8_t    = L.i8_type context in
  let i1_t    = L.i1_type context in
  let void_t  = L.void_type context in
  let string_t = L.pointer_type i8_t in
  
  let main_ftype = L.function_type void_t [| i32_t |] in
  let the_module = L.create_module context "uCurry" in
  let ltype_of_type = function 
    | A.INT_TY          -> i32_t
    | A.BOOL_TY         -> i1_t
    | A.STRING_TY       -> string_t
    | _                 -> raise (CODEGEN_NOT_YET_IMPLEMENTED "ltype_of_type") in
    (* | A.LIST_TY t       -> 
    | A.UNIT_TY         -> 
    | A.FUNCTION_TY (t1, t2) ->
    | A.CONSTRUCTOR_TY s -> 
    | A.TUPLE_TY taus    ->  *)

  let main_function = L.define_function "main" main_ftype the_module in
  let builder = L.builder_at_end context (L.entry_block main_function) in

  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
  and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder
  and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in
  
  (* let string_pool = StringMap.empty in TODO *)

  let lookup n varmap = StringMap.find n varmap in
  
  let rec exprWithVarmap builder varmap = (* TODO: delete *) 
  let rec expr builder = function 
  (* (ty, exp) = match exp with bracket *)
    | S.SLiteral (_, Construct (name, value)) -> raise (CODEGEN_NOT_YET_IMPLEMENTED "constructor literal")
    | S.SLiteral (_, INT i) -> L.const_int i32_t i 
    | S.SLiteral (_, STRING s) -> L.build_global_stringptr s "str" builder
    | S.SLiteral (_, BOOL b) -> raise (CODEGEN_NOT_YET_IMPLEMENTED "boolean literal")
    | S.SLiteral (_, LIST values)  -> raise (CODEGEN_NOT_YET_IMPLEMENTED "list literal")
    | S.SLiteral (_, TUPLE values) -> raise (CODEGEN_NOT_YET_IMPLEMENTED "tuple literal")
    | S.SLiteral (_, INF_LIST i) -> raise (CODEGEN_NOT_YET_IMPLEMENTED "inf list")
    | S.SLiteral (_, UNIT) -> L.const_null void_t (* TOOD: double check unit value *) 
    | S.SVar     (_, name) ->
        L.build_load (lookup name varmap) name builder
    | S.SAssign  (_, name, e) -> 
        let e' = expr builder e in
        let _ = L.build_store e' (lookup name varmap) builder in e'
    | S.SApply _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "APPLY")
    | S.SIf (_, pred, then_expr, else_expr) ->raise (CODEGEN_NOT_YET_IMPLEMENTED "APPLY") 
    | S.SLet (_, bindings, exp) ->
      let newvarsWithNames = List.map (fun ((tau, name), e) -> 
                                        let e' = expr builder e in 
                                        let newvar = L.build_alloca (ltype_of_type tau) name builder in 
                                        let _ = L.build_store e' newvar builder in
                                        (newvar, name))
                                      bindings in 
      let newvarmap = List.fold_left (fun varmap (newvar, name) -> 
                                      StringMap.add name newvar varmap)
                                      varmap
                                      newvarsWithNames
      in exprWithVarmap builder newvarmap exp 
    | S.SBegin _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "Begin")
    | S.SBinop (_, e1, binop, e2) -> 
      let e1'= expr builder e1 
      and e2' = expr builder e2 
    in (match binop with
    | A.Add     -> L.build_add e1' e2' "temp" builder 
    | A.Sub     -> L.build_sub e1' e2' "temp" builder 
    | A.Mult    -> L.build_mul e1' e2' "temp" builder 
    | A.Div     -> L.build_sdiv e1' e2' "temp" builder 
    | A.Mod     -> L.build_srem e1' e2' "temp" builder 
    | A.And     -> L.build_and e1' e2' "temp" builder 
    | A.Or      -> L.build_or e1' e2' "temp" builder 
    | A.Equal   -> L.build_icmp L.Icmp.Eq e1' e2' "temp" builder 
    | A.Neq     -> L.build_icmp L.Icmp.Ne e1' e2' "temp" builder 
    | A.Less    -> L.build_icmp L.Icmp.Slt e1' e2' "temp" builder 
    | A.Leq     -> L.build_icmp L.Icmp.Sle e1' e2' "temp" builder 
    | A.Greater -> L.build_icmp L.Icmp.Sgt e1' e2' "temp" builder 
    | A.Geq     -> L.build_icmp L.Icmp.Sge e1' e2' "temp" builder 
    | A.Cons    -> raise (CODEGEN_NOT_YET_IMPLEMENTED "cons") 
    )
    | S.SUnop (_, unop, e) -> (
        let printf_t : L.lltype =
          L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
        in
        let printf_func : L.llvalue =
          L.declare_function "printf" printf_t the_module
        in
        let e' = expr builder e in
        match (unop, e) with
        | A.Print, S.SLiteral (_, INT _) ->

            L.build_call printf_func [| int_format_str; e' |] "printf" builder
        | A.Print, S.SLiteral (_, STRING _) ->
            L.build_call printf_func
              [| string_format_str; e' |]
              "printf" builder
        (* | A.Print, S.SBinop  (Ast.INT_TY, e1, binop, e2)
        | A.Print, S.SBinop  (Ast.BOOL_TY, e1, binop, e2)) *)
        
        | _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "A"))
    | S.SLambda _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "Lambda")
    | S.SCase _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "Case")
    | S.SNoexpr -> L.const_null void_t (* TOOD: double check noexpr value *) 
    | _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "catchall")
  in
  expr builder
  in

  (* varmap is the variable environment *)
  let rec stmt builder varmap = function
    | S.SVariable (tau, name, e) -> (* Handle string -> create a global string pointer and assign the global name to the name *)
        let e' = exprWithVarmap builder varmap e in
        let newvar = L.build_alloca (ltype_of_type tau) name builder in 
        let varmap' = StringMap.add name newvar varmap in 
        let _ = L.build_store e' (lookup name varmap) builder in 
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
  let main_builder, _ = List.fold_left 
                        (fun (_, varmap) def -> stmt builder varmap def ) 
                        (builder, StringMap.empty) defs 
  in 

  (* Add a void return to main *)
  let _ = add_terminal main_builder L.build_ret_void in 

  the_module
