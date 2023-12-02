module S = Sast
module A = Ast
open Util
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type match_tree = 
  | Node of A.expr * match_tree * match_tree
  | Leaf of A.expr

let rec generate_exp : match_tree -> A.expr = function 
  | Leaf e -> e 
  | Node (condition, then_tree, else_tree) -> 
      let then_expr = generate_exp then_tree 
      and else_expr = generate_exp else_tree in 
      A.If (condition, then_expr, else_expr)

let generate_tree (scrutinee: A.expr) 
                  (cases: A.case_expr list) 
                  (default: A.expr) : match_tree =
    match cases with 
      | [] -> Leaf default 
      | [ (CONCELL (hd_name, tl_name), con_e); (NIL, nil_e) ] 
      | [ (NIL, nil_e); (CONCELL (hd_name, tl_name), con_e)] ->
          let cond = A.Unop (A.IsNull, scrutinee)
          and then_case = nil_e 
          and else_case = 
            let hd_binding = (hd_name, A.Unop (A.Hd, scrutinee))
            and tl_binding = (tl_name, A.Unop (A.Tl, scrutinee)) in 
            (A.Let ([ hd_binding; tl_binding ], con_e))
          in 
          Node (cond, Leaf then_case, Leaf else_case)
      | _ -> Leaf default (* TODO: not yet implement other pattern matching *)


let case_convert (scrutinee : A.expr) (cases: A.case_expr list) (default: A.expr): A.expr = 
  generate_exp (generate_tree scrutinee cases default) 
  (* Note: the current default case would preserve the case-of node of non-list pattern matching, given that the default is the case-expr itself *)
  
            



