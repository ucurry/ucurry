module S = Sast
module A = Ast
open Util
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type match_tree = Node of A.expr * match_tree * match_tree | Leaf of A.expr

let rec bind_pat (scrutinee : A.expr) (pat : A.pattern) : (string * A.expr) list
    =
  match pat with
  | A.VAR_PAT x -> [ (x, scrutinee) ]
  | A.WILDCARD -> []
  | A.NIL -> []
  | A.CONCELL (hd, tl) ->
      let hd_binding = A.Unop (A.Hd, scrutinee)
      and tl_binding = A.Unop (A.Tl, scrutinee) in
      [ (hd, hd_binding); (tl, tl_binding) ]
  | A.CON_PAT (_, pat) -> bind_pat (A.Unop (A.GetField, scrutinee)) pat
  | A.PATTERNS pats ->
      let bindings =
        List.mapi (fun idx pat -> bind_pat (A.At (scrutinee, idx)) pat) pats
      in
      List.concat bindings

let desugar (scrutinee : A.expr) (cases : A.case_expr list) :
    A.case_expr list =
  let add_bind case =
    let pat, e = case in
    let binds = bind_pat scrutinee pat in
    match binds with [] -> (pat, e) | _ -> (pat, A.Let (binds, e))
  in
  List.map add_bind cases

let rec generate_exp : match_tree -> A.expr = function
  | Leaf e -> e
  | Node (condition, then_tree, else_tree) ->
      let then_expr = generate_exp then_tree
      and else_expr = generate_exp else_tree in
      A.If (condition, then_expr, else_expr)

let generate_tree (scrutinee : A.expr) (cases : A.case_expr list)
    (default : A.expr) : match_tree =
  let cases' = desugar scrutinee cases in 
  match cases' with
  | [] -> Leaf default
  | [ (CONCELL _, con_e); (NIL, nil_e) ]
  | [ (NIL, nil_e); (CONCELL _, con_e) ] ->
      let cond = A.Unop (A.IsNull, scrutinee) in
      Node (cond, Leaf nil_e, Leaf con_e)
  | _ -> Leaf (A.Case (scrutinee, cases)) (* TODO: not yet implement other pattern matching *)

let case_convert (scrutinee : A.expr) (cases : A.case_expr list)
    (default : A.expr) : A.expr =
  generate_exp (generate_tree scrutinee cases default)
(* Note: the current default case would preserve the case-of node of non-list pattern matching, given that the default is the case-expr itself *)
