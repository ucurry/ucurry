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

module Pattern : Map.OrderedType with type t = A.pattern = struct
  type t = A.pattern
  let compare p1 p2 =
    if match_all p1 && match_all p2 then 0
    else String.compare (A.string_of_pattern p1) (A.string_of_pattern p2)
end

module PatMap = Map.Make (Pattern)
module PatSet = Set.Make (Pattern)

let rec is_exhaustive (vcon_sets : S.vcon_sets) (vcon_env : S.vcon_env)
    (scrutinee_tau : typ) (pats : A.pattern list) =
  let exhaust = is_exhaustive vcon_sets vcon_env in
  if List.exists match_all pats then ()
  else
    match scrutinee_tau with
    | TUPLE_TY _ ->
        ()
        (* HACK : too much work to check exhaustive pattern on tuple *)
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
        (* TODO: list pattern check is not exhaustive *)
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
let rec get_binds (scrutinee : A.expr) (pat : Ast.pattern) :
    (string * A.expr) list =
  match pat with
  | A.CON_PAT (name, pat) -> get_binds (A.GetField (scrutinee, name)) pat
  | A.VAR_PAT a -> [ (a, scrutinee) ]
  | A.WILDCARD | A.NIL -> []
  | A.CONCELL (hd, tl) ->
      List.append
        (get_binds (A.Unop (A.Hd, scrutinee)) hd)
        (get_binds (A.Unop (A.Tl, scrutinee)) tl)
  | A.PATS ps ->
      let bindings =
        List.mapi (fun idx p -> get_binds (A.At (scrutinee, idx)) p) ps
      in
      List.concat bindings

(* raise error if the pattern is illegal *)
let legal_pats tau pats vcon_env =
  let rec legal_pat t p =
    match (t, p) with
    | _, A.WILDCARD -> ()
    | _, A.VAR_PAT _ -> ()
    | LIST_TY tau, A.CONCELL (hd, tl) ->
        ignore (legal_pat tau hd);
        ignore (legal_pat t tl)
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
        if match_all p 
          then match rest with [] ->(acc, Some e) 
        | _ -> raise (SU.TypeError "some cases are unused")
        else remove_unmatch_acc rest ((p, e) :: acc)
  in
  let scrutinee ,  default =  remove_unmatch_acc cases [] in 
  List.rev scrutinee, default


let repeat_check (pat_groups : A.case_expr list PatMap.t) (idx : int) : unit =
  let extract_pat p =
    match p with
    | A.PATS ps -> A.PATS (Util.drop ps idx)
    | _ -> raise (Impossible "repeat check on non tuple pattern")
  in
  let to_avoid =
    match PatMap.find_opt A.WILDCARD pat_groups with
    | Some cases ->
        let add_pat acc (p, _) = PatSet.add (extract_pat p) acc in
        List.fold_left add_pat PatSet.empty cases
    | None -> PatSet.empty
  in
  let check_avoid = function
    | A.WILDCARD, _ -> ()
    | _, cases ->
        let pats, _ = List.split cases in
        let to_check = List.map extract_pat pats in
        let no_dup =
          List.fold_left
            (fun acc p ->
              match PatSet.find_opt p acc with
              | Some _ -> raise (SU.TypeError "some pattern is not used")
              | None -> PatSet.add p acc)
            PatSet.empty to_check
        in
        if PatSet.is_empty (PatSet.inter to_avoid no_dup) then ()
        else raise (SU.TypeError "some cases are unused in pattern matching")
  in
  List.iter check_avoid (PatMap.bindings pat_groups)

let rec simplify_pat p =
  match p with
  | A.WILDCARD -> A.WILDCARD
  | A.VAR_PAT _ -> A.WILDCARD
  | A.CONCELL (p1, p2) -> A.CONCELL (simplify_pat p1, simplify_pat p2)
  | A.PATS ps -> if match_all p then A.WILDCARD else A.PATS (List.map simplify_pat ps)
  | A.NIL -> A.NIL
  | A.CON_PAT (name, p) -> A.CON_PAT (name, simplify_pat p)

(* add inner let binding for each case expression, remove unused case, and return a default expression
   from an all matched expression if there is any *)
let desugar (scrutinee : A.expr) (cases : A.case_expr list) : A.case_expr list =
  let pats, _ = List.split cases in
  let add_let (p, e) =
    match get_binds scrutinee p with [] -> e | bs -> A.Let (bs, e)
  in
  List.combine (List.map simplify_pat pats) (List.map add_let cases)

type tree = Test of (A.expr * tree) list * tree | Leaf of A.expr | Failure

let rec string_repeat s n =
  match n with 0 -> s | _ -> s ^ string_repeat s (n - 1)

let rec print_tree n tree =
  let space = string_repeat "\t" n in
  let print_with_space s = print_string @@ space ^ s in
  let print_condition e =
    match e with
    | A.Binop (_, A.Equal, A.Literal s) | A.Binop (A.Literal s, A.Equal, _) ->
        print_with_space (A.string_of_literal s ^ "?")
    | A.Unop (A.IsNull, _) -> print_with_space "null?"
    | A.Unop (A.Not, A.Unop (A.IsNull, _)) -> print_with_space "cons"
    | _ -> print_with_space @@ A.string_of_expr e
  in
  match tree with
  | Test (tests, default) ->
      let print_single (cond, tree) =
        print_newline ();
        print_with_space "TEST -->";
        print_condition cond;
        print_tree (n + 1) tree
      in
      List.iter print_single tests;
      print_with_space " DEFAULT ";
      print_tree (n + 1) default
  | Failure -> print_with_space "faiure"
  | Leaf e ->
      print_string "  MATCHED ";
      print_endline @@ " " ^ A.string_of_expr e

let rec compile_tree tree =
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
    | A.CONCELL (hd, tl) ->
        let matched' = match_cons scrutinee hd tl matched resume in
        let cond = A.Unop (A.Not, A.Unop (A.IsNull, scrutinee)) in
        Test ([ (cond, matched') ], resume)
    | A.NIL ->
        let cond = A.Unop (A.IsNull, scrutinee) in
        Test ([ (cond, matched) ], resume)
    | A.PATS ps -> match_pats scrutinee 0 ps matched resume
    | A.VAR_PAT _ -> raise (Util.Impossible "no variable pattern after desugar")
    | A.WILDCARD -> matched
  and match_pats (tuple : A.expr) (idx : int) (pats : A.pattern list)
      (matched : tree) (resume : tree) : tree =
    if List.length pats = idx then matched
    else
      match List.nth pats idx with
      | A.WILDCARD | A.VAR_PAT _ ->
          match_pats tuple (idx + 1) pats matched resume
      | p ->
          let continue = match_pats tuple (idx + 1) pats matched resume in
          match_pat (A.At (tuple, idx)) p continue resume
  and match_cons (cons : A.expr) (hd_pat : A.pattern) (tl_pat : A.pattern)
      (matched : tree) (default : tree) : tree =
    let match_tl = match_pat (A.Unop (A.Tl, cons)) tl_pat matched default in
    match_pat (A.Unop (A.Hd, cons)) hd_pat match_tl default
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
    | TUPLE_TY taus ->
        let rec match_tuple idx cases : unit =
          (* get the current pattern that is aimed to be matched *)
          let get_cur_pat pat =
            match pat with
            | A.PATS sub_pats -> List.nth sub_pats idx
            | pat ->
                raise
                  (Util.Impossible
                     ("tuple type always have tuple pattern "
                    ^ A.string_of_pattern pat))
          in
          if idx == List.length taus - 1 then ()
          else
            (* add one (pat * expr) case expression to the collected based on idx *)
            let add_one_case collected (pat, e) =
              let curr_pat = get_cur_pat pat in
              match PatMap.find_opt curr_pat collected with
              | Some cases -> PatMap.add curr_pat ((pat, e) :: cases) collected
              | None -> PatMap.add curr_pat [ (pat, e) ] collected
            in
            (* pat_to_rest organize the list case expression based on idx*)
            let pat_to_rest = List.fold_left add_one_case PatMap.empty cases in
            repeat_check pat_to_rest (idx + 1);
            List.iter  (fun (_, pats) -> match_tuple (idx + 1) pats) (PatMap.bindings pat_to_rest)
        in
        match_tuple 0 shrinked_cases;
        (match shrinked_cases with
        | [] -> nearest_resume
        | (p, matched_e) :: rest ->
            let resume' = match_resume scrutinee rest tau nearest_resume in
            match_pat scrutinee p (Leaf matched_e) resume')
    | LIST_TY _ -> (
        match shrinked_cases with
        | [] -> nearest_resume
        | (p, matched_e) :: rest ->
            let resume' = match_resume scrutinee rest tau nearest_resume in
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
