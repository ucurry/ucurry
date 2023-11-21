module L = Llvm
module A = Ast
module S = Sast
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
    | A.UNIT_TY -> i1_t
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

  let string_pool =
    let rec mk_expr_string_pool pool builder (_, sx) =
      match sx with
      | S.SLiteral (A.STRING s) -> (
          match StringMap.find_opt s pool with
          | Some _ -> pool
          | None ->
              StringMap.add s (L.build_global_stringptr s "strlit" builder) pool
          )
      | S.SLiteral _ -> pool
      | S.SVar _ -> pool
      | S.SAssign (_, sexpr) -> mk_expr_string_pool pool builder sexpr
      | S.SApply (func, args) ->
          let pool' = mk_expr_string_pool pool builder func in
          List.fold_left
            (fun poolacc sexpr -> mk_expr_string_pool poolacc builder sexpr)
            pool' args
      | S.SIf (condition, texpr, eexpr) ->
          let pool' = mk_expr_string_pool pool builder condition in
          let pool'' = mk_expr_string_pool pool' builder texpr in
          mk_expr_string_pool pool'' builder eexpr
      | S.SLet (bindings, sexpr) ->
          let pool' =
            List.fold_left
              (fun poolacc (_, subsexpr) ->
                mk_expr_string_pool poolacc builder subsexpr)
              pool bindings
          in
          mk_expr_string_pool pool' builder sexpr
      | S.SBegin sexprs ->
          List.fold_left
            (fun poolacc sexpr -> mk_expr_string_pool poolacc builder sexpr)
            pool sexprs
      | S.SBinop (operand1, _, operand2) ->
          let pool' = mk_expr_string_pool pool builder operand1 in
          mk_expr_string_pool pool' builder operand2
      | S.SUnop (_, operand) -> mk_expr_string_pool pool builder operand
      | S.SLambda (_, sexpr) -> mk_expr_string_pool pool builder sexpr
      | S.SCase (scrutinee, patterns) ->
          let pool' = mk_expr_string_pool pool builder scrutinee in
          List.fold_left
            (fun poolacc (_, sexpr) ->
              mk_expr_string_pool poolacc builder sexpr)
            pool' patterns
      | S.SNoexpr -> pool
    in
    let mk_defs_string_pool pool builder sdef =
      match sdef with
      | S.SFunction (_, sexpr) -> mk_expr_string_pool pool builder sexpr
      | S.SDatatype _ -> pool
      | S.SVal (_, _, sexpr) -> mk_expr_string_pool pool builder sexpr
      | S.SExp sexpr -> mk_expr_string_pool pool builder sexpr
      | S.SCheckTypeError _ -> pool
    in
    List.fold_left
      (fun poolacc sdef -> mk_defs_string_pool poolacc builder sdef)
      StringMap.empty defs
  in
  let lookup n varmap = StringMap.find n varmap in
 
  let rec getFormalTypes = function
    | A.FUNCTION_TY (formalty, retty) -> formalty :: getFormalTypes retty
    | _ -> []
  in
  let rec getRetType = function
    | A.FUNCTION_TY (_, retty) -> getRetType retty
    | retty -> retty
  in
  let deconstructSLambda (ty, se) = (* NOTE: didn't check if ty is actually a funty  *)
    match se with
    | S.SLambda (formals, body) -> (getFormalTypes ty, getRetType ty, formals, body)
    | _ -> raise (SHOULDNT_RAISED "not an slambda type")
  in

  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
    | Some _ -> ()
    | None -> ignore (instr builder)
  in

  let rec exprWithVarmap builder varmap =
    let rec expr builder (ty, top_exp) =
      match top_exp with
      | S.SLiteral (Construct (name, value)) ->
          raise (CODEGEN_NOT_YET_IMPLEMENTED "constructor literal")
      | S.SLiteral (INT i) -> L.const_int i32_t i
      | S.SLiteral (STRING s) -> StringMap.find s string_pool
      | S.SLiteral (BOOL b) -> L.const_int i1_t (if b then 1 else 0)
      | S.SLiteral (LIST values) ->
          raise (CODEGEN_NOT_YET_IMPLEMENTED "list literal")
      | S.SLiteral (TUPLE values) ->
          raise (CODEGEN_NOT_YET_IMPLEMENTED "tuple literal")
      | S.SLiteral (INF_LIST i) ->
          raise (CODEGEN_NOT_YET_IMPLEMENTED "inf list")
      | S.SLiteral UNIT ->
          L.const_int i1_t 0
          (* L.const_null void_t  *)
          (* TOOD: double check unit value *)
      | S.SVar name ->
          L.build_load (lookup name varmap) name
            builder (* %a1 = load i32, i32* %a, align 4 *)
      | S.SAssign (name, e) ->
          let e' = expr builder e in
          let _ = L.build_store e' (lookup name varmap) builder in
          e'
      | S.SApply ((ft, f), args) -> (
          let fretty = getRetType ft in
          let llargs = List.rev (List.map (expr builder) (List.rev args)) in
          match f with
          | S.SVar fname ->
              let fdef = StringMap.find fname varmap in
              let result = match fretty with A.UNIT_TY -> "" | _ -> fname ^ "_result" in
              L.build_call fdef (Array.of_list llargs) result builder
          | S.SLambda _ -> 
              let e'= expr builder (ft, f) in
              let result = match fretty with A.UNIT_TY -> "" | _ -> "lambda" ^ "_result" in
              L.build_call e' (Array.of_list llargs) result builder 
          | _ -> raise (SHOULDNT_RAISED "Illegal function application"))
      | S.SIf (pred, then_expr, else_expr) ->
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
      | S.SBegin sexprs ->
          List.fold_left
            (fun _ sexpr -> expr builder sexpr)
            (L.const_int i1_t 0)
            sexprs (*TODO: Double check with team + Jank as hell*)
      (* | S.SBegin _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "Begin") *)
      | S.SBinop (e1, binop, e2) -> (
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
      | S.SLambda _ ->
          let (e', _) = generateFunction varmap "lambda" (ty, top_exp) in (* No need to worry about naming - LLVM will take care of it *)
          e'
      | S.SCase _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "Case")
      | S.SNoexpr -> L.const_null void_t (* TOOD: double check noexpr value *)
    in
    expr builder
  
  and generateFunction varmap name slambda =
    (* NOTE: current goal is to write parallel arguments - no automatic curry but can take in more than one arg *)
    let formaltypes, retty, formals, body = deconstructSLambda slambda in
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
    let localvarmap = (* TODO: need to reimplement after closure convert ! *)
      List.fold_left2 add_formal varmap formalsandtypes
        (Array.to_list (L.params the_function))
    in
    let e' = exprWithVarmap builder localvarmap body in
    let _ = add_terminal builder (fun b -> L.build_ret e' b) in
    (the_function, varmap')
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
