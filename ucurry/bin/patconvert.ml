open Typing
module A = Ast
module P = Past
module StringMap = Map.Make(String)
module CC = Caseconvert
let typ_env : typ StringMap.t = failwith "todo"
let rec type_of exp typ_env : typ =  failwith "todo"

let rec match_all (p : Ast.pattern) =
  match p with
  | A.WILDCARD -> true
  | A.VAR_PAT _ -> true
  | A.CON_PAT _ -> false
  | A.PATS ps -> List.for_all match_all ps

(* a => a + 2 becomes a => let a = scrutinee in a + 2 *)
let get_binds (scrutinee : A.expr) (pats : A.pattern) : (string * A.expr) list =
  failwith "todo"

(* add inner let binding for each case expression, remove unused case, and return a default expression
   from an all matched expression if there is any *)
let desugar (scrutinee : A.expr) (cases : A.case_expr list) :
    A.case_expr list * A.expr option =
  let pats, _ = List.split cases in
  let add_let (p, e) = A.Let (get_binds scrutinee p, e) in
  let cases' = List.combine pats (List.map add_let cases) in
  let rec remove_unmatched (cases : A.case_expr list) (acc : A.case_expr list) =
    match cases with
    | [] -> (acc, None)
    | (p, e) :: rest ->
        if match_all p then (acc, Some e)
        else remove_unmatched rest ((p, e) :: acc)
  in
  remove_unmatched cases' []

type tree =
  | Test of (P.expr * tree) list * tree option
  | Choice of P.expr * tree * tree
  | Leaf of P.expr

let rec compile_tree tree =
  match tree with
  | Leaf e -> e
  | Choice (cond, e1, e2) ->
      let then_exp = compile_tree e1 and else_exp = compile_tree e2 in
      P.If (cond, then_exp, else_exp)
  | Test ([], Some e) -> compile_tree e
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
      P.If (cond, then_exp, else_exp)
  | Test ((cond, tree') :: rest, default) ->
      let then_exp = compile_tree tree'
      and else_exp = compile_tree (Test (rest, default)) in
      P.If (cond, then_exp, else_exp)

let rec match_compile (scrutinee : A.expr) (cases : A.case_expr list) : tree =
  (* Assuming that the list of cases is exahustive, or it does not matter if it is exhasutive *)
  let rec match_resume (scrutinee : A.expr) (cases : A.case_expr list) (resume : A.expr option) : tree =
    let tau = type_of scrutinee typ_env in 
    (match tau with 
      CONSTRUCTOR_TY _ -> 
       let vcon_to_cases : (A.case_expr list StringMap.t)  =  failwith "cases for each matched vcon" in 
       let build_sub_tree (vcon: vcon_name) (sub_cases: A.case_expr list) : tree  =  failwith "subtree" in 
       let vcons, cases = List.split @@ StringMap.bindings vcon_to_cases in 
       
     failwith "" 
    | TUPLE_TY _ -> failwith "" 
    | LIST_TY _ -> failwith "" 
    | _ ->  failwith "" 
    )
    in 
  let desugared, default = desugar scrutinee cases in
  match_resume scrutinee desugared default


let rec case_convert (scrutinee : A.expr) (cases: A.case_expr list) = 
  compile_tree @@ match_compile scrutinee cases 