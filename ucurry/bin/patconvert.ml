open Typing
open Util
module S = Sast
module StringMap = Map.Make (String)
module CC = Caseconvert

type arg_typ = typ
type ret_typ = typ

let get_tau = fst

let rec match_all (p : Ast.pattern) =
  match p with
  | A.WILDCARD -> true
  | A.VAR_PAT _ -> true
  | A.CON_PAT _ -> false
  | A.PATS ps -> List.for_all match_all ps

(* a => a + 2 becomes a => let a = scrutinee in a + 2 *)
let rec get_binds (scrutinee : S.sexpr) (pat : Ast.pattern)
    (vcon_env : S.vcon_env) : (string * S.sexpr) list =
  let rec bind_pats scrutinee pat =
    match pat with
    | A.CON_PAT (name, pat) ->
        let _, vcon_id, arg_typ = StringMap.find name vcon_env in
        bind_pats (arg_typ, S.SGetField (scrutinee, vcon_id)) pat
    | A.VAR_PAT a -> (
        match scrutinee with
        | _, S.SGetField _ | _, S.SAt _ -> [ (a, scrutinee) ]
        | tau, _ ->
            let thunk =
              (FUNCTION_TY (UNIT_TY, tau), S.SLambda ([ "unit" ], scrutinee))
            in
            [ (a, thunk) ])
    | A.WILDCARD -> []
    | A.PATS ps ->
        let taus =
          match get_tau scrutinee with
          | TUPLE_TY taus -> taus
          | _ -> raise (SemantUtil.TypeError "expected to be a tuple type")
        in
        let bindings =
          List.mapi
            (fun idx (p, t) -> bind_pats (t, S.SAt (scrutinee, idx)) p)
            (List.combine ps taus)
        in
        List.concat bindings
  in
  bind_pats scrutinee pat

let  is_exhaustive pats tau = true
(* add inner let binding for each case expression, remove unused case, and return a default expression
   from an all matched expression if there is any *)
let desugar (scrutinee : S.sexpr) (cases : S.scase_expr list)
    (vcon_env : S.vcon_env) : S.scase_expr list * S.sexpr option =
  let pats, _ = List.split cases in
  let add_let (p, e) =
    (get_tau e, S.SLet (get_binds scrutinee p vcon_env, e))
  in
  let cases' = List.combine pats (List.map add_let cases) in
  let rec remove_unmatched (cases : S.scase_expr list) (acc : S.scase_expr list)
      =
    match cases with
    | [] -> (acc, None)
    | (p, e) :: rest ->
        if match_all p then (acc, Some e)
        else remove_unmatched rest ((p, e) :: acc)
  in
  remove_unmatched cases' []

type tree =
  | Test of (S.sexpr * tree) list * tree option
  | Choice of S.sexpr * tree * tree
  | Leaf of S.sexpr

let rec compile_tree tree =
  match tree with
  | Leaf e -> e
  | Choice (cond, e1, e2) ->
      let then_exp = compile_tree e1 and else_exp = compile_tree e2 in
      (get_tau then_exp, S.SIf (cond, then_exp, else_exp))
  | Test ([], Some default) -> compile_tree default
  | Test ([], None) ->
      failwith
        "decision treee should never have a test node with no branch and no \
         default tree"
  | Test (_ :: [], None) ->
      failwith
        "decision tree should never have a test node with only one branch and \
         no default tree"
  | Test ([ (cond, tree') ], Some default) ->
      let then_exp = compile_tree tree' and else_exp = compile_tree default in
      (get_tau else_exp, S.SIf (cond, then_exp, else_exp))
  | Test ((cond, tree') :: rest, default) ->
      let then_exp = compile_tree tree'
      and else_exp = compile_tree (Test (rest, default)) in
      (get_tau else_exp, S.SIf (cond, then_exp, else_exp))

let rec match_compile 
    (scrutinee : S.sexpr) 
    (cases : S.scase_expr list)
    (vcon_env : S.vcon_env) : tree =
  (* Assuming that the list of cases is exahustive, or it does not matter if it is exhasutive *)
  let rec match_resume 
      (scrutinee : S.sexpr) 
      (cases : S.scase_expr list)
      (resume : tree option) : tree =
    match get_tau scrutinee with
    | CONSTRUCTOR_TY _ ->
        let vcon_to_cases : S.scase_expr list StringMap.t =
          failwith "cases for each matched vcon"
        in
        let build_sub_tree (vcon, sub_cases) : S.sexpr * tree =
          let _, vcon_id, arg_tau = StringMap.find vcon vcon_env in
          let cond =
            ( BOOL_TY,
              S.SBinop
                ((STRING_TY, S.SGetTag scrutinee),
                  A.Equal,
                  (STRING_TY, S.SLiteral (S.STRING vcon))))
          in
          (cond,
            match_resume
              (arg_tau, S.SGetField (scrutinee, vcon_id))
              sub_cases resume )  
              (* TODO: check for resume *)
        in
        Test (List.map build_sub_tree (StringMap.bindings vcon_to_cases), None)
    | TUPLE_TY _ -> failwith "pattern matching for tuple has not been implemented"
    | LIST_TY _ -> failwith "pattern matching for list has not been implemented"
    | INT_TY | STRING_TY | BOOL_TY | UNIT_TY | FUNCTION_TY _ | ANY_TY -> (
        match cases with
        | (Ast.WILDCARD, e) :: _ -> Leaf e
        | (Ast.VAR_PAT _, e) :: _ -> Leaf e
        | _ ->
            raise
              (SemantUtil.TypeError "atomic type matched to invalid pattern"))
  in
  let desugared, default = desugar scrutinee cases vcon_env in
  match default with
  | Some e -> match_resume scrutinee desugared (Some (Leaf e))
  | None -> match_resume scrutinee desugared None

let rec case_convert 
    (scrutinee : S.sexpr) 
    (cases : S.scase_expr list)
    (vcon_env : S.vcon_env) =
  compile_tree @@ match_compile scrutinee cases vcon_env
