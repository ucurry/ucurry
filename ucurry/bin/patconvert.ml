open Typing
open Util
module S = Sast
module StringMap = Map.Make (String)
module SU = SemantUtil

type arg_typ = typ
type ret_typ = typ

(* HACK: since user defined datatype has been changed *)
let de_thunk = function
  | FUNCTION_TY (UNIT_TY, ret_typ) -> ret_typ
  | _ ->
      raise
        (Impossible "constructor arg type should be function type after lazy")

let rec match_all (p : Ast.pattern) =
  match p with
  | A.WILDCARD -> true
  | A.VAR_PAT _ -> true
  | A.CON_PAT _ -> false
  | A.PATS ps -> List.for_all match_all ps

(* a => a + 2 becomes a => let a = scrutinee in a + 2 *)
let rec get_binds (scrutinee : A.expr) (pat : Ast.pattern) :
    (string * A.expr) list =
  let rec bind_pats scrutinee pat =
    match pat with
    | A.CON_PAT (name, pat) -> bind_pats (A.GetField (scrutinee, name)) pat
    | A.VAR_PAT a -> (
        match scrutinee with
        | A.GetField _ | A.At _ -> [ (a, scrutinee) ]
        | _ -> [ (a, A.Thunk scrutinee) ])
    | A.WILDCARD -> []
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
    | CONSTRUCTOR_TY name, A.CON_PAT (vcon_name, pat) ->
        let dt_name, _, arg_typ = StringMap.find vcon_name vcon_env in
        if dt_name = name then legal_pat (de_thunk arg_typ) pat
        else
          raise
            (SU.TypeError
               ("illegal constructor pattern: expeceted  " ^ name
              ^ " datatype, got " ^ dt_name ^ "data type"))
    | TUPLE_TY taus, A.PATS ps -> (
        try
          ignore (List.map2 legal_pat (List.map de_thunk taus) ps);
          ()
        with Invalid_argument _ ->
          raise (SU.TypeError "tuple pattern and scrutine length unmatch"))
    | _, A.CON_PAT _ ->
        raise
          (SU.TypeError
             ("illugal use of constructor pattern: got scrutinee type "
            ^ string_of_typ tau ^ " and pattern: " ^ A.string_of_pattern p))
    | _, A.PATS _ ->
        raise
          (SU.TypeError
             ("illegal use of tuple pattern: " ^ "got scrutinee type "
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
  let add_let (p, e) = match get_binds scrutinee p with 
    [] -> e 
    | bs -> A.Let (bs, e) in 
  List.combine pats (List.map add_let cases)

type tree =
  | Test of (A.expr * tree) list * tree option
  | Choice of A.expr * tree * tree
  | Leaf of A.expr

let rec print_tree = function 
 | Test (tests, default) -> 
    print_string "TEST-NODE{";
    print_newline ();
    let print_single (cond, tree) = 
    print_string "TEST-CONDITION";
      print_string (A.string_of_expr cond) ; 
      print_newline ();
      print_tree tree; 
      print_newline ();
    in List.iter print_single tests; 
      print_string "DEFAULT:" ; 
      (match default with None -> print_string "none" | Some tree -> print_tree tree);
      print_string "}";
      prerr_newline ();
  | Leaf e -> (
      print_string " LEAF-NODE {"; 
      print_string @@ A.string_of_expr e; 
      print_string "}"; 
      print_newline ();)
  | Choice (cond, tree1, tree2)  -> (
    print_string "CHOICE-NODE {";
    print_newline ();
    print_string (A.string_of_expr cond) ; 
    print_newline ();
    print_tree tree1; 
    print_tree tree2; 
    print_string "}"; 
    print_newline ();)

let rec compile_tree tree =
  print_tree tree;
  match tree with
  | Leaf e -> e
  | Choice (cond, e1, e2) ->
      let then_exp = compile_tree e1 and else_exp = compile_tree e2 in
      A.If (cond, then_exp, else_exp)
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
      A.If (cond, then_exp, else_exp)
  | Test ((cond, tree') :: rest, default) ->
      let then_exp = compile_tree tree'
      and else_exp = compile_tree (Test (rest, default)) in
      A.If (cond, then_exp, else_exp)

let rec match_compile (scrutinee : A.expr) (cases : A.case_expr list)
    (vcon_env : S.vcon_env) (tau : typ) : tree =
  (* Assuming that the list of cases is exahustive *)
  let rec match_resume 
      (scrutinee : A.expr) 
      (cases : A.case_expr list)
      (tau : typ) (resume : tree option) : tree =
    let shrinked_cases, default = remove_unmatch cases in
    let nearest_resume =
      match default with Some e -> Some (Leaf e) | None -> None
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
        ( match nearest_resume with 
          Some _ ->  Test (List.map build_sub_tree (StringMap.bindings vcon_to_cases), nearest_resume)
        | None ->   Test (List.map build_sub_tree (StringMap.bindings vcon_to_cases), resume))

    | TUPLE_TY _ ->
        failwith "pattern matching for tuple has not been implemented"
    | LIST_TY _ -> failwith "pattern matching for list has not been implemented"
    | INT_TY | STRING_TY | BOOL_TY | UNIT_TY | FUNCTION_TY _ | ANY_TY -> 
      (match default 
       with Some e -> Leaf e
       | None -> (match resume with Some tree -> tree 
                  | None -> raise (Impossible "atomic type should have fall throw case")))
  in
  let desugared = desugar scrutinee cases in
  match_resume scrutinee desugared tau None

let rec case_convert (scrutinee : A.expr) (cases : A.case_expr list)
    (vcon_env : S.vcon_env) (tau : typ) =
  compile_tree @@ match_compile scrutinee cases vcon_env tau
