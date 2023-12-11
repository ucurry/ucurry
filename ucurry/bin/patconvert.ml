open Typing
open Util
module S = Sast
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)
module SU = SemantUtil

type arg_typ = typ
type ret_typ = typ

let rec match_all (p : Ast.pattern) =
  match p with
  | A.WILDCARD -> true
  | A.VAR_PAT _ -> true
  | A.CON_PAT _ | A.NIL | A.CONCELL _ -> false
  | A.PATS ps -> List.for_all match_all ps

let rec is_exhaustive (vcon_sets : S.vcon_sets) (vcon_env : S.vcon_env)
    (scrutinee_tau : typ) (pats : A.pattern list) =
  let exhaust = is_exhaustive vcon_sets vcon_env in
  if List.exists match_all pats then ()
  else
    match scrutinee_tau with
    | TUPLE_TY _ ->
        ()
        (* HACK : too much work to check exhaustive pattern on tuple *)
        (* let add_patterns matrix p  = match p with
             A.PATTERNS ps -> ps :: matrix
           | A.WILDCARD -> matrix
           | A.VAR_PAT _ -> matrix
           | _ -> raise (TypeError ("tuple cannot be matched on " ^ A.string_of_pattern p)) in
           let pat_matric = List.fold_left add_patterns [] pats in *)
    | CONSTRUCTOR_TY name ->
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
        let remain_pat, collected =
          collect_all pats must_have StringMap.empty
        in
        let check_sub_pat name pats =
          let _, _, arg_typ = StringMap.find name vcon_env in
          exhaust arg_typ pats
        in
        if StringSet.is_empty remain_pat then
          StringMap.iter check_sub_pat collected
        else raise (SU.TypeError "pattern matching non-ehaustive")
    | INT_TY -> raise (SU.TypeError "an integer is not matched to any pattern")
    | LIST_TY _ ->
        let must_have =
          StringSet.add "cons" (StringSet.add "nil" StringSet.empty)
        in
        let rec has_all pats must_have =
          match pats with
          | [] -> must_have
          | A.CONCELL (_, _) :: rest ->
              has_all rest (StringSet.remove "cons" must_have)
          | A.NIL :: rest -> has_all rest (StringSet.remove "nil" must_have)
          | _ -> must_have
        in
        if StringSet.is_empty (has_all pats must_have) then ()
        else raise (SU.TypeError "pattern matching non exhastive for list")
    | BOOL_TY -> raise (SU.TypeError "an boolean is not matched to any pattern")
    | STRING_TY ->
        raise (SU.TypeError "an string is not matched to any pattern")
    | UNIT_TY -> raise (SU.TypeError "cannot pattern match on unit")
    | FUNCTION_TY _ -> raise (SU.TypeError "cannot pattern match on function")
    | ANY_TY -> raise (SU.TypeError "any type cannot pattern match on function")

(* a => a + 2 becomes a => let a = scrutinee in a + 2 *)
let get_binds (scrutinee : A.expr) (pat : Ast.pattern) : (string * A.expr) list
    =
  let rec bind_pats scrutinee pat =
    match pat with
    | A.CON_PAT (name, pat) -> bind_pats (A.GetField (scrutinee, name)) pat
    | A.VAR_PAT a -> [ (a, scrutinee) ]
    | A.WILDCARD | A.NIL -> []
    | A.CONCELL (hd, tl) ->
        [ (hd, A.Unop (A.Hd, scrutinee)); (tl, A.Unop (A.Tl, scrutinee)) ]
    | A.PATS ps ->
        let bindings =
          List.mapi (fun idx p -> bind_pats (A.At (scrutinee, idx)) p) ps
        in
        List.concat bindings
  in
  bind_pats scrutinee pat

(* raise error if the pattern is illegal *)
let legal_pats tau pats vcon_env =
  let rec legal_pat t p =
    match (t, p) with
    | _, A.WILDCARD -> ()
    | _, A.VAR_PAT _ -> ()
    | LIST_TY _, A.CONCELL _ -> ()
    | LIST_TY _, A.NIL -> ()
    | CONSTRUCTOR_TY name, A.CON_PAT (vcon_name, pat) ->
        let dt_name, _, arg_typ = StringMap.find vcon_name vcon_env in
        if dt_name = name then legal_pat arg_typ pat
        else
          raise
            (SU.TypeError
               ("illegal constructor pattern: expeceted  " ^ name
              ^ " datatype, got " ^ dt_name ^ "data type"))
    | TUPLE_TY taus, A.PATS ps -> (
        try
          ignore (List.map2 legal_pat taus ps);
          ()
        with Invalid_argument _ ->
          raise (SU.TypeError "tuple pattern and scrutine length unmatch"))
    | _, A.CON_PAT _ | _, A.PATS _ | _, A.CONCELL _ | _, A.NIL ->
        raise
          (SU.TypeError
             ("illegal use of pattern: " ^ "got scrutinee type "
            ^ string_of_typ tau ^ " and pattern: " ^ A.string_of_pattern p))
  in
  ignore (List.map (legal_pat tau) pats);
  ()

(*  return a shrinked case list and an optional default case *)
let remove_unmatch cases =
  let rec remove_unmatch_acc (cases : A.case_expr list) (acc : A.case_expr list)
      =
    match cases with
    | [] -> (acc, None)
    | (p, e) :: rest ->
        if match_all p then (acc, Some e)
        else remove_unmatch_acc rest ((p, e) :: acc)
  in
  remove_unmatch_acc cases []

(* add inner let binding for each case expression, remove unused case, and return a default expression
   from an all matched expression if there is any *)
let desugar (scrutinee : A.expr) (cases : A.case_expr list) : A.case_expr list =
  let pats, _ = List.split cases in
  let add_let (p, e) =
    match get_binds scrutinee p with [] -> e | bs -> A.Let (bs, e)
  in
  List.combine pats (List.map add_let cases)

type tree = Test of (A.expr * tree) list * tree | Leaf of A.expr | Failure

let rec string_repeat s n =
  match n with 0 -> s | _ -> s ^ string_repeat s (n - 1)

let rec print_tree n tree =
  let space = string_repeat "  " n in
  let print_with_space s = print_endline @@ space ^ s in
  print_with_space "{";
  (match tree with
  | Test (tests, default) ->
      let print_single (cond, tree) =
        print_with_space @@ " TEST-CONDITION" ^ A.string_of_expr cond;
        print_tree (n + 1) tree
      in
      List.iter print_single tests;
      print_with_space " DEFAULT:";
      print_tree (n + 1) default;
      print_with_space "}"
  | Failure -> print_with_space "faiure"
  | Leaf e ->
      print_with_space " LEAF-NODE ";
      print_with_space @@ " " ^ A.string_of_expr e;
      print_newline ());
  print_with_space "}"

let rec compile_tree tree =
  (* print_tree 0 tree; *)
  match tree with
  | Leaf e -> e
  | Test ([], default) -> compile_tree default
  | Test ([ (cond, tree') ], default) ->
      let then_exp = compile_tree tree' and else_exp = compile_tree default in
      A.If (cond, then_exp, else_exp)
  | Test ((cond, tree') :: rest, default) ->
      let then_exp = compile_tree tree'
      and else_exp = compile_tree (Test (rest, default)) in
      A.If (cond, then_exp, else_exp)
  | Failure -> raise (Impossible "impossible")

let match_compile (scrutinee : A.expr) (cases : A.case_expr list)
    (vcon_env : S.vcon_env) (vcon_sets : S.vcon_sets) (tau : typ) : tree =
  (* Assuming that the list of cases is exahustive *)
  let rec match_pat (scrutinee : A.expr) (pat : A.pattern) (matched : tree)
      (resume : tree) : tree =
    match pat with
    | A.CON_PAT (name, pat') ->
        let matched' =
          match_pat (A.GetField (scrutinee, name)) pat' matched resume
        in
        let cond =
          A.Binop (A.GetTag scrutinee, A.Equal, A.Literal (A.STRING name))
        in
        Test ([ (cond, matched') ], resume)
    | A.CONCELL _ ->
        let cond = A.Unop (A.Not, A.Unop (A.IsNull, scrutinee)) in
        Test ([ (cond, matched) ], resume)
    | A.NIL ->
        let cond = A.Unop (A.IsNull, scrutinee) in
        Test ([ (cond, matched) ], resume)
    | A.PATS ps -> match_pats scrutinee 0 ps matched resume
    | A.VAR_PAT _ -> matched
    | A.WILDCARD -> matched
  and match_pats (tuple : A.expr) (idx : int) (pats : A.pattern list)
      (matched : tree) (default : tree) : tree =
    if List.length pats = idx then matched
    else
      match List.nth pats idx with
      | A.WILDCARD | A.VAR_PAT _ ->
          match_pats tuple (idx + 1) pats matched default
      | p ->
          let continue = match_pats tuple (idx + 1) pats matched default in
          match_pat (A.At (tuple, idx)) p continue default
  in
  let rec match_resume (scrutinee : A.expr) (cases : A.case_expr list)
      (tau : typ) (resume : tree) : tree =
    let shrinked_cases, default = remove_unmatch cases in
    let nearest_resume =
      match default with Some e -> Leaf e | None -> resume
    in
    match tau with
    | CONSTRUCTOR_TY _ ->
        let add_one_case collected (pat, e) =
          match pat with
          | A.CON_PAT (name, p) -> (
              match StringMap.find_opt name collected with
              | Some cases -> StringMap.add name ((p, e) :: cases) collected
              | None -> StringMap.add name [ (p, e) ] collected)
          | _ ->
              raise
                (Impossible
                   "construtor type should only have constructoer pattern in \
                    match compile")
        in
        let vcon_to_cases : A.case_expr list StringMap.t =
          List.fold_left add_one_case StringMap.empty shrinked_cases
        in
        let build_sub_tree (vcon_name, sub_cases) : A.expr * tree =
          let cond =
            A.Binop (A.GetTag scrutinee, A.Equal, A.Literal (A.STRING vcon_name))
          in
          let _, _, arg_typ = StringMap.find vcon_name vcon_env in
          ( cond,
            match_resume
              (A.GetField (scrutinee, vcon_name))
              (List.rev sub_cases) arg_typ nearest_resume )
        in
        Test
          ( List.map build_sub_tree (StringMap.bindings vcon_to_cases),
            nearest_resume )
    | TUPLE_TY _ | LIST_TY _ -> (
        match cases with
        | [] -> resume
        | [ (p, matched_e) ] -> match_pat scrutinee p (Leaf matched_e) resume
        | (p, matched_e) :: rest ->
            let resume' = match_resume scrutinee rest tau resume in
            match_pat scrutinee p (Leaf matched_e) resume')
    | INT_TY | STRING_TY | BOOL_TY | UNIT_TY | FUNCTION_TY _ | ANY_TY -> (
        match default with Some e -> Leaf e | None -> resume)
  in
  let desugared = desugar scrutinee cases in
  let cases, default = remove_unmatch desugared in
  let ps, _ = List.split cases in
  let resume =
    match default with
    | Some e -> Leaf e
    | None ->
        let _ = is_exhaustive vcon_sets vcon_env tau ps in
        let default_err =
          A.Begin
            [
              A.Unop (A.Println, A.Literal (A.STRING "Case not matched"));
              A.NoMatch;
            ]
        in
        Leaf default_err
  in

  match_resume scrutinee desugared tau resume

let case_convert (scrutinee : A.expr) (cases : A.case_expr list)
    (vcon_env : S.vcon_env) (vcon_sets : S.vcon_sets) (tau : typ) =
  compile_tree @@ match_compile scrutinee cases vcon_env vcon_sets tau
