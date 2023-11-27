module S = Sast
module A = Ast
open Util

type match_tree =
  | Node of Sast.sexpr * match_tree * match_tree
  | Leaf of Sast.sexpr

let rec generate_exp : match_tree -> Sast.sexpr = 
  function Leaf e -> e 
    | Node (condition, then_tree, else_tree) -> 
      let then_expr = generate_exp then_tree and 
          else_tree = generate_exp else_tree in
      let tau, _ = then_expr in  
      tau, Sast.SIf (condition, then_expr, else_tree)

let generate_tree 
        (scrutinee : S.sexpr)
        (cases : S.scase_expr list) 
        (default : S.sexpr) : match_tree = 
        let scrutinee_tau , _ = scrutinee in 
        let convert cases = match cases with
        | [] -> Leaf default 
        | [(Sast.CONCELL (hd, tl), con_e) ; (NIL, nil_e) ] | 
          [(NIL, nil_e) ; (Sast.CONCELL (hd, tl), con_e)] ->  
            let cond = A.BOOL_TY, S.SUnop (A.IsNull, scrutinee) and 
                then_exp = nil_e and 
                else_exp = 
                let subtype = list_subtype scrutinee_tau in
                let hd_binding = ((subtype, hd), (subtype, S.SUnop (A.Hd, scrutinee)))
                and tl_binding = ((scrutinee_tau, tl), (scrutinee_tau, S.SUnop (A.Tl, scrutinee))) 
                and body_tau, _ = con_e in 
                body_tau, S.SLet ([hd_binding; tl_binding], con_e)
          in  Node (cond, Leaf then_exp, Leaf else_exp)
        | _ -> raise (Impossible "not implemented ")
        in convert cases

let case_convert (_ : A.typ) (scrutinee : S.sexpr)
    (cases : S.scase_expr list) (default : S.sexpr) : S.sexpr  
    = generate_exp (generate_tree scrutinee cases default)