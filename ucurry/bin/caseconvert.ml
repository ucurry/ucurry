module S = Sast
module A = Ast
open Util

(* Unused && Not Tested  *)

let rec case_convert (ret_tau : A.typ) (scrutinee : S.sexpr)
    (cases : S.scase_expr list) (default : S.sexpr) : S.sexpr =
  let scrutinee_type, _ = scrutinee in
  let continue_covert rest = case_convert ret_tau scrutinee rest default in
  match cases with
  | [] -> default
  | (pat, e) :: rest -> (
      match pat with
      | VAR_PAT name ->
          (ret_tau, S.SLet ([ ((scrutinee_type, name), scrutinee) ], e))
      | WILDCARD -> e
      | NIL ->
          ( ret_tau,
            S.SIf
              ( (A.BOOL_TY, S.SUnop (A.IsNull, scrutinee)),
                e,
                continue_covert rest ) )
      | CONCELL (hd, tl) ->
          let subtype = list_subtype scrutinee_type in
          let hd_binding = ((subtype, hd), (subtype, S.SUnop (A.Hd, scrutinee)))
          and tl_binding =
            ((scrutinee_type, tl), (scrutinee_type, S.SUnop (A.Tl, scrutinee)))
          in
          let matched_exp = S.SLet ([ hd_binding; tl_binding ], e) in
          ( ret_tau,
            S.SIf
              ( (A.BOOL_TY, S.SUnop (A.IsNull, scrutinee)),
                continue_covert rest,
                (ret_tau, matched_exp) ) )
      | CON_PAT _ -> e (* TODO: place holder - not implemneted yet  *))
