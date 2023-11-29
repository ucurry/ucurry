module S = Sast
module A = Ast
open Util
open SemantUtil
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type match_tree =
  | Node of Sast.sexpr * match_tree * match_tree
  | Leaf of Sast.sexpr

let rec generate_exp : match_tree -> Sast.sexpr = function
  | Leaf e -> e
  | Node (condition, then_tree, else_tree) ->
      let then_expr = generate_exp then_tree
      and else_tree = generate_exp else_tree in
      let tau, _ = then_expr in
      (tau, Sast.SIf (condition, then_expr, else_tree))

(* let desugar (scrutinee : S.sexpr) (cases: S.scase_expr list) : S.scase_expr list =
   let remove_var case = match case with
   | (Sast.PATTERNS ps, e) -> e
   | (Sast.VAR_PAT s, e) -> let *)

let generate_tree (scrutinee : S.sexpr) (cases : S.scase_expr list)
    (default : S.sexpr) : match_tree =
  let scrutinee_tau, _ = scrutinee in
  let convert cases =
    match cases with
    | [] -> Leaf default
    | [ (Sast.CONCELL (hd, tl), con_e); (NIL, nil_e) ]
    | [ (NIL, nil_e); (Sast.CONCELL (hd, tl), con_e) ] ->
        let cond = (A.BOOL_TY, S.SUnop (A.IsNull, scrutinee))
        and then_exp = nil_e
        and else_exp =
          let subtype = list_subtype scrutinee_tau in
          let hd_binding = ((subtype, hd), (subtype, S.SUnop (A.Hd, scrutinee)))
          and tl_binding =
            ((scrutinee_tau, tl), (scrutinee_tau, S.SUnop (A.Tl, scrutinee)))
          and body_tau, _ = con_e in
          (body_tau, S.SLet ([ hd_binding; tl_binding ], con_e))
        in
        Node (cond, Leaf then_exp, Leaf else_exp)
    | _ ->
        Leaf
          default (* TODO: not implemented for other pattern matching cases  *)
  in
  convert cases

let case_convert (_ : A.typ) (scrutinee : S.sexpr) (cases : S.scase_expr list)
    (default : S.sexpr) : S.sexpr =
  generate_exp (generate_tree scrutinee cases default)

let rec is_exhaustive (vcon_sets : S.vcon_sets) (type_env : S.type_env)
    (scrutinee_tau : A.typ) (pats : A.pattern list) =
  let exhaust = is_exhaustive vcon_sets type_env in
  let call_all_fall_through ps =
    List.for_all
      (fun p -> match p with A.WILDCARD | A.VAR_PAT _ -> true | _ -> false)
      ps
  in
  let rec can_fall_through = function
    | [] -> false
    | A.WILDCARD :: _ -> true
    | A.VAR_PAT _ :: _ -> true
    | A.PATTERNS ps :: rest -> call_all_fall_through ps || can_fall_through rest
    | _ :: rest -> can_fall_through rest
  in
  if can_fall_through pats then ()
  else
    match scrutinee_tau with
    | A.TUPLE_TY _ ->
        (* HACK : too much work to check exhaustive pattern on tuple *)
        (* let add_patterns matrix p  = match p with
             A.PATTERNS ps -> ps :: matrix
           | A.WILDCARD -> matrix
           | A.VAR_PAT _ -> matrix
           | _ -> raise (TypeError ("tuple cannot be matched on " ^ A.string_of_pattern p)) in
           let pat_matric = List.fold_left add_patterns [] pats in *)
        raise
          (TypeError
             "A pattern matching expression for a tuple must have a fall \
              through case")
    | A.CONSTRUCTOR_TY name ->
        let must_have = StringMap.find name vcon_sets in
        let rec collect_all pats must_have collected =
          match pats with
          | [] -> (must_have, collected)
          | A.CON_PAT (name, p) :: rest ->
              let must_have' = StringSet.remove name must_have
              and collected' =
                match StringMap.find_opt name collected with
                | Some ps -> StringMap.add name (p :: ps) collected
                | None -> StringMap.add name [ p ] collected
              in
              collect_all rest must_have' collected'
          | _ :: rest -> collect_all rest must_have collected
        in
        let remain_pat, collected = collect_all pats must_have empty_env in
        let check_sub_pat name pats =
          let arg_tau, _ = findFunctionType name type_env in
          exhaust arg_tau pats
        in
        if StringSet.is_empty remain_pat then
          StringMap.iter check_sub_pat collected
        else raise (TypeError "some cases are not matched")
    | A.LIST_TY _ ->
        let must_have = StringSet.add "cons" (StringSet.add "nil" empty_set) in
        let rec has_all pats must_have =
          match pats with
          | [] -> must_have
          | A.CONCELL (_, _) :: rest ->
              has_all rest (StringSet.remove "cons" must_have)
          | A.NIL :: rest -> has_all rest (StringSet.remove "nil" must_have)
          | _ -> must_have
        in
        if StringSet.is_empty (has_all pats must_have) then ()
        else raise (TypeError "pattern matching non exhastive for list")
    | A.INT_TY -> raise (TypeError "an integer is not matched to any pattern")
    | A.BOOL_TY -> raise (TypeError "an boolean is not matched to any pattern")
    | A.STRING_TY -> raise (TypeError "an string is not matched to any pattern")
    | A.UNIT_TY -> raise (TypeError "cannot pattern match on unit")
    | A.FUNCTION_TY _ -> raise (TypeError "cannot pattern match on function")
