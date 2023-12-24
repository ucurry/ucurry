(* Code Generation (CAST -> LLVM-IR) *)
(* Authors: Stephanie Xu, Vivian Li, Matt Zhou *)

open Typing
module L = Llvm
module A = Ast
module C = Cast
module U = Util
module CGUtil = Cgutil
module StringMap = Map.Make (String)

exception CODEGEN_NOT_YET_IMPLEMENTED of string
exception SHOULDNT_RAISED of string
exception REACHED of string

let get_dt_name = function
  | CONSTRUCTOR_TY dt_name -> dt_name
  | _ -> raise (U.Impossible "not a constructortype")

let build_main_body defs =
  let context = L.global_context () in
  let i32_t = L.i32_type context in
  let i8_t = L.i8_type context in
  let i1_t = L.i1_type context in
  let void_ptr = L.pointer_type (L.i8_type context) in
  let null_captured_param = L.const_null (L.pointer_type i8_t) in
  let main_ftype = L.function_type i32_t [| i32_t |] in
  let the_module = L.create_module context "uCurry" in
  let deconstructClosure (ty, se) =
    (* NOTE: didn't check if ty is actually a funty  *)
    match se with
    | C.Closure ((formals, body), cap) ->
        let formalty, retty = Util.get_ft ty in
        ([ formalty ], retty, formals, body, cap)
    | _ -> raise (SHOULDNT_RAISED "not an C.closure type")
  in
  let ty_map = CGUtil.build_datatypes context the_module defs in
  let ltype_of_type = CGUtil.ltype_of_type ty_map the_module context in
  let lambda_name = "lambda" in
  let main_function = L.define_function "main" main_ftype the_module in
  let builder = L.builder_at_end context (L.entry_block main_function) in
  let int_format_str = L.build_global_stringptr "%d" "fmt" builder
  and string_format_str = L.build_global_stringptr "%s" "fmt" builder
  and int_nl_format_str = L.build_global_stringptr "%d\n" "fmt" builder
  and string_nl_format_str = L.build_global_stringptr "%s\n" "fmt" builder
  and unit_str = L.build_global_stringptr "()" "unit" builder 
  and unit_strln = L.build_global_stringptr "()\n" "unitln" builder 
  and string_pool = CGUtil.build_string_pool defs builder in
  let build_lit = CGUtil.build_literal context string_pool in
  let lookup n varmap = StringMap.find n varmap in
  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
    | Some _ -> ()
    | None -> ignore (instr builder)
  in

  let delay_fun_t = L.function_type void_ptr [||] in 
  let thunk_t = 
    L.struct_type context [| L.pointer_type delay_fun_t; void_ptr; i1_t |]
  in

  let force_t = L.function_type void_ptr [| L.pointer_type thunk_t |] in
  let force_func = L.declare_function "Force" force_t the_module in 
   
  let rec exprWithVarmap builder captured_param varmap =
    let rec expr builder (ty, top_exp) =
      match top_exp with
      | C.Literal l -> build_lit l
      | C.Var name -> L.build_load (lookup name varmap) name builder
      | C.Apply (((ft, _) as sf), args) ->
          let fun_closure = expr builder sf in
          let fdef = U.get_data_field 0 fun_closure builder "fdef" in
          let cap = U.get_data_field 1 fun_closure builder "cap" in
          let fretty = U.get_retty ft in
          let llargs =
            cap :: List.rev (List.map (expr builder) (List.rev args))
          in

          let result =
            match fretty with UNIT_TY -> "" | _ -> "apply" ^ "_result"
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
              (fun vm (name, e) ->
                let tau, _ = e in
                let e' = expr builder e in
                let reg = L.build_alloca (ltype_of_type tau) name builder in
                let vm' = StringMap.add name reg vm in
                let _ = L.build_store e' (lookup name vm') builder in
                vm')
              varmap bindings
          in
          exprWithVarmap builder captured_param varmap' exp
      | C.Letrec (bindings, exp) ->
          let builder', varmap' =
            List.fold_left
              (fun (builder, vm) (name, (tau, e)) ->
                match e with
                | C.Closure (lambda, cap) ->
                    build_named_function tau name lambda cap captured_param vm
                      builder
                | _ -> failwith "impossible letrec not binds with closure")
              (builder, varmap) bindings
          in
          exprWithVarmap builder' captured_param varmap' exp
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
              | INT_TY | BOOL_TY | STRING_TY | UNIT_TY ->
                  L.build_icmp L.Icmp.Eq e1' e2' "temp" builder
              | _ ->
                  raise
                    (SHOULDNT_RAISED
                       "other equality type not supported (should raise Type error)"))
          | A.Neq -> L.build_icmp L.Icmp.Ne e1' e2' "temp" builder
          | A.Less -> L.build_icmp L.Icmp.Slt e1' e2' "temp" builder
          | A.Leq -> L.build_icmp L.Icmp.Sle e1' e2' "temp" builder
          | A.Greater -> L.build_icmp L.Icmp.Sgt e1' e2' "temp" builder
          | A.Geq -> L.build_icmp L.Icmp.Sge e1' e2' "temp" builder)
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
          | A.Print, INT_TY ->
              L.build_call printf_func [| int_format_str; e' |] "printf" builder
          | A.Print, STRING_TY ->
              L.build_call printf_func
                [| string_format_str; e' |]
                "printf" builder
          | A.Println, INT_TY ->
              L.build_call printf_func
                [| int_nl_format_str; e' |]
                "printf" builder
          | A.Println, STRING_TY ->
              L.build_call printf_func
                [| string_nl_format_str; e' |]
                "printf" builder
          | A.Println, BOOL_TY ->
              L.build_call printf_func
                [| int_nl_format_str; e' |]
                "printf" builder
          | A.Print, BOOL_TY ->
              L.build_call printf_func [| int_format_str; e' |] "printf" builder
          | A.Print, UNIT_TY ->
              L.build_call printf_func [| unit_str |] "printf" builder 
          | A.Println, UNIT_TY ->
              L.build_call printf_func [| unit_strln |] "printf" builder
          | A.Print, _ -> raise (SHOULDNT_RAISED "printing non-primitve type value not supported (should raise type error)")
          | A.Println, _ -> raise (SHOULDNT_RAISED "printing non-primitve type value not supported (should raise type error)")
          | A.Neg, _ -> L.build_neg e' "temp" builder
          | A.Not, _ -> L.build_not e' "temp" builder
          | A.Hd, _ ->
              let list_ptr = expr builder inner_e in
              Util.get_data_field 0 list_ptr builder "hd_field"
          | A.Tl, _ ->
              let list_ptr = expr builder inner_e in
              Util.get_data_field 1 list_ptr builder "tl_field"
          | A.IsNull, _ ->
              let list_ptr = expr builder inner_e in
              (* let list = L.build_load list_ptr "list_content" builder in  *)
              (* if L.is_null list_ptr then L.const_int i1_t 1 else L.const_int i1_t 0  *)
              L.build_is_null list_ptr "null?" builder
          | _ -> raise (CODEGEN_NOT_YET_IMPLEMENTED "unop"))
      | C.Captured index ->
          U.get_data_field index captured_param builder "capvar"
      | C.Closure (_, cap) ->
          (* Alloc function *)
          let funtype, function_ptr = alloc_function lambda_name ty in
          let cl_struct_type =
            L.struct_type context [| L.pointer_type funtype; void_ptr |]
          in
          let closure_ptr =
            L.build_malloc cl_struct_type "fun_closure" builder
          in
          let capstruct_type, capstruct_ptr =
            build_captured_struct builder varmap captured_param cap
          in
          let casted_capstruct_ptr =
            L.build_bitcast capstruct_ptr void_ptr "capstruct" builder
          in
          let _ = U.set_data_field function_ptr 0 closure_ptr builder in
          let _ = U.set_data_field casted_capstruct_ptr 1 closure_ptr builder in
          let _ =
            build_function_body function_ptr capstruct_type (ty, top_exp)
          in
          closure_ptr
      | C.Construct ((i, vcon_name), arg) ->
          let dt_name = get_dt_name ty in
          let dt_struct_type = StringMap.find dt_name ty_map in
          let dt_struct = L.build_malloc dt_struct_type dt_name builder in
          let tag_v = StringMap.find vcon_name string_pool in
          let field_v = expr builder arg in
          ignore (U.set_data_field field_v i dt_struct builder);
          ignore (U.set_data_field tag_v 0 dt_struct builder);
          dt_struct
      | C.Tuple ses ->
          let tuple_ptr_ty = ltype_of_type ty in
          let tuple_ty = L.element_type tuple_ptr_ty in
          let tuple_ptr = L.build_malloc tuple_ty "tuple" builder in
          let tuple_vs = List.map (expr builder) ses in
          let set_field v i = U.set_data_field v i tuple_ptr builder in
          ignore (U.map_i set_field 0 tuple_vs);
          tuple_ptr
      | C.At (e, i) ->
          let tuple_ptr = expr builder e in
          Util.get_data_field i tuple_ptr builder "tuple field"
      | C.Noexpr -> L.const_int i1_t 0
      | C.GetTag e ->
          let str_ptr = expr builder e in
          Util.get_data_field 0 str_ptr builder "tag"
      | C.GetField (e, i) ->
          let str_ptr = expr builder e in
          Util.get_data_field i str_ptr builder "fi"
      | C.Nomatch -> L.undef (ltype_of_type ty)
      | C.EmptyList _ ->
          let list_ptr_ty = ltype_of_type ty in
          L.const_null list_ptr_ty
      | C.List (hd, tl) ->
          let list_ty = L.element_type (ltype_of_type ty) in
          let hd_v = expr builder hd in
          let tl_ptr = expr builder tl in
          let list_ptr = L.build_malloc list_ty "list_ptr" builder in
          ignore (Util.set_data_field hd_v 0 list_ptr builder);
          ignore (Util.set_data_field tl_ptr 1 list_ptr builder);
          list_ptr
      | C.Thunk delay_fun -> 
      (* failwith "not yet implement thunk" *)
           (* malloc thunk struct *)
          let thunk_ptr = L.build_malloc thunk_t "thunk_sturct" builder in 

          (* put the delay_fun into the thunk struct *)
          let delay_funp = exprWithVarmap builder captured_param varmap delay_fun in 
          let casted_delay_funp = L.build_bitcast delay_funp (L.pointer_type (delay_fun_t)) "casted_delay_fun" builder in
          let _ = U.set_data_field casted_delay_funp 0 thunk_ptr builder in (* set delay fun *)
          (* TODO HERE: the delay_funp does not match the universal funp in the thunk struct -> need casting *)
          let _ = U.set_data_field (L.const_null void_ptr) 1 thunk_ptr builder in  (* set val *)
          let _ = U.set_data_field (L.const_int i1_t 0) 2 thunk_ptr builder in (* set evaled to false *)

          (* return struct pointer *)
          thunk_ptr
          (* let reg = L.build_alloca (L.pointer_type thunk_t) "thunk" builder in 
          let _ = L.build_store thunk_ptr reg builder in 
          reg *)

      | C.Force e -> 
      (* failwith "not yet implement force" *)
          (* TODO: need to generate the code for this function once -> so that not repeatedly generate it *)
          (* Call force function *)
          let e' = expr builder e in 
          let ret = L.build_call force_func [| e' |] "Force" builder in 

          (* bistcast a void pointer to int *)
          let casted_ret = L.build_ptrtoint ret (L.i64_type context) "casted_ret" builder in (* void pointer to i64 *)
          L.build_trunc_or_bitcast casted_ret (ltype_of_type ty) "trunc_ret" builder
          
    in
    expr builder
  and alloc_function name fun_tau =
    let formaltype, retty = Util.get_ft fun_tau in
    (* Add a void ptr as a placeholder for captured variables *)
    let formal_lltypes = void_ptr :: List.map ltype_of_type [ formaltype ] in
    let ftype =
      L.function_type (ltype_of_type retty) (Array.of_list formal_lltypes)
    in
    (* Note: ftype and fun_tau are not matched after this point*)
    let the_function = L.define_function name ftype the_module in
    (ftype, the_function)
  and build_captured_struct builder varmap captured_param cap =
    (* Get the values and types of the captured list *)
    let captured_values =
      List.map (exprWithVarmap builder captured_param varmap) cap
    in
    let captured_types = Array.of_list (List.map L.type_of captured_values) in
    let struct_type = L.struct_type context captured_types in
    let struct_ptr = L.build_malloc struct_type "captured_struct" builder in
    ignore
      (U.map_i
         (fun v i -> U.set_data_field v i struct_ptr builder)
         0 captured_values);
    (struct_type, struct_ptr)
  and build_function_body the_function captured_type closure : unit =
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* Cast the cap struct param and load its value *)
    let void_cap_param = L.param the_function 0 in
    let void_cap_name = L.value_name void_cap_param in
    let void_local_cap = L.build_alloca void_ptr void_cap_name builder in
    let _ = L.build_store void_cap_param void_local_cap builder in
    let void_local_cap' = L.build_load void_local_cap void_cap_name builder in
    let cap_param =
      L.build_bitcast void_local_cap'
        (L.pointer_type captured_type)
        "cap" builder
    in

    let localvarmap = StringMap.empty in
    let formaltypes, _, formals, body, _ = deconstructClosure closure in
    let formal_lltypes = List.map ltype_of_type formaltypes in
    let formalsandtypes = List.combine formal_lltypes formals in
    (* build the function body *)
    let add_formal m (t, n) p =
      let _ = L.set_value_name n p in
      let local = L.build_alloca t n builder in
      let _ = L.build_store p local builder in
      StringMap.add n local m (* return a new local varmap *)
    in
    let localvars =
      let l = Array.to_list (L.params the_function) in
      match l with _ :: rest -> rest | _ -> failwith "Impossible"
    in
    let localvarmap' =
      List.fold_left2 add_formal localvarmap formalsandtypes localvars
    in
    let e' = exprWithVarmap builder cap_param localvarmap' body in
    ignore (add_terminal builder (fun b -> L.build_ret e' b))
  (* varmap is the variable environment that maps (variable : string |---> to reg : llvale) *)
  and build_named_function tau name lambda cap captured_param varmap builder =
    let funtype, function_ptr = alloc_function name tau in
    (* Alloc the closure struct and adds it into varmap *)
    let cl_struct_type =
      L.struct_type context [| L.pointer_type funtype; void_ptr |]
    in
    let closure_ptr = L.build_malloc cl_struct_type "fun_closure" builder in
    let closure_pp =
      L.build_alloca (L.pointer_type cl_struct_type) "closurepp" builder
    in
    let _ = L.build_store closure_ptr closure_pp builder in
    let varmap' = StringMap.add name closure_pp varmap in
    (* Populate the capture struct  *)
    let capstruct_type, capstruct_ptr =
      build_captured_struct builder varmap' captured_param cap
    in
    let casted_capstruct_ptr =
      L.build_bitcast capstruct_ptr void_ptr "capstruct" builder
    in

    (* Populate the closure struct *)
    let _ = U.set_data_field function_ptr 0 closure_ptr builder in
    let _ = U.set_data_field casted_capstruct_ptr 1 closure_ptr builder in

    (* Build the function body *)
    ignore
      (build_function_body function_ptr capstruct_type
         (tau, C.Closure (lambda, cap)));
    (builder, varmap')
  in
  let stmt builder varmap = function
    | C.Val (name, e) ->
(*     
        (* malloc thunk struct *)
        let thunk_ptr = L.build_malloc thunk_t "thunk_sturct" builder in 

        (* put the delay_fun into the thunk struct *)
        let e' = exprWithVarmap builder null_captured_param varmap e in 
        let _ = U.set_data_field e' 0 thunk_ptr builder in (* set delay fun *)
        let _ = U.set_data_field (L.const_pointer_null i1_t) 1 thunk_ptr builder in  (* set val *)
        let _ = U.set_data_field (L.const_int i1_t 0) 2 thunk_ptr builder in (* set evaled to false *)

      (* return struct pointer *)
        let reg = L.build_alloca (L.pointer_type thunk_t) name builder in 
        let _ = L.build_store thunk_ptr reg builder in 
        let varmap' = StringMap.add name reg varmap in 
        (builder, varmap') *)

        let tau, _ = e in
        let reg = L.build_alloca (ltype_of_type tau) name builder in
        let varmap' = StringMap.add name reg varmap in
        let e' = exprWithVarmap builder null_captured_param varmap e in
        let _ = L.build_store e' reg builder in
        (builder, varmap')
    | C.Function (tau, name, (lambda, cap)) ->
        build_named_function tau name lambda cap null_captured_param varmap
          builder
    | C.Exp e ->
        let _ = exprWithVarmap builder null_captured_param varmap e in
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

  (* Add a return 0 to main *)
  let _ = add_terminal main_builder (L.build_ret (L.const_int i32_t 0)) in

  the_module
