(* open Typing  *)
module A = Ast
module P = Past
module StringMap = Map.Make (String)

let case_convert (program : A.def list) : P.def list =
  (* TODO: double check type casting *)
  let rec gen (scrutinee : A.expr) (ces : P.case_expr list) (resume : P.expr) :
      P.expr =
    match ces with
    | [] -> resume
    | (hd, e) :: rest -> (
        match hd with
        | A.VAR_PAT n -> P.Let ([ (n, case_exp scrutinee) ], e)
        | A.WILDCARD -> e
        | A.CON_PAT (vcon, p) ->
            let cond_exp =
              P.Binop
                ( P.GetTag (case_exp scrutinee),
                  A.Equal,
                  P.Literal (A.STRING vcon) )
            in
            let then_exp =
              gen
                (A.GetField (scrutinee, vcon))
                [ (p, e) ]
                (gen scrutinee rest resume)
            in
            let else_exp = gen scrutinee rest resume in
            P.If (cond_exp, then_exp, else_exp)
        | A.PATS [] -> e
        | A.PATS ps ->
            let re' = gen scrutinee rest resume in
            let compute_new_e acc_e i p =
              gen (A.At (scrutinee, i)) [ (p, acc_e) ] re'
            in
            Util.fold_right_i compute_new_e e 0 ps
            (* let rest_ces = [(A.WILDCARD, A.Case (scrutinee, rest))] in
               let new_case acc_e i p =
                   A.Case (A.At (scrutinee, i), (p,acc_e)::rest_ces)
               in
               let new_e =
                 Util.fold_right_i new_case e 0 ps
               in
               case_exp new_e *))
  and case_exp : A.expr -> P.expr = function
    | A.Literal v -> P.Literal v
    | A.Construct (vcon_name, arg) -> P.Construct (vcon_name, case_exp arg)
    | A.Case (_, []) ->
        failwith "at least one case in pattern matching should be provided"
    | A.Case (scrutinee, ces) ->
        let ps, es = List.split ces in
        let es' = List.map case_exp es in
        let ces' = List.combine ps es' in
        gen scrutinee ces' P.Nomatch
    | A.GetTag e -> P.GetTag (case_exp e)
    | A.GetField (e, vc) -> P.GetField (case_exp e, vc)
    | A.Unop (u, e) -> P.Unop (u, case_exp e)
    | A.Binop (e1, binop, e2) -> P.Binop (case_exp e1, binop, case_exp e2)
    | A.Begin es -> P.Begin (List.map case_exp es)
    | A.If (e1, e2, e3) -> P.If (case_exp e1, case_exp e2, case_exp e3)
    | A.Let (bindings, e) ->
        let names, es = List.split bindings in
        let es' = List.map case_exp es in
        let bindings' = List.combine names es' in
        P.Let (bindings', case_exp e)
    | A.Var n -> P.Var n
    | A.Assign _ -> failwith " assign impossible "
    | A.Apply (e, es) -> P.Apply (case_exp e, List.map case_exp es)
    | A.Lambda (tau, arg_names, body) -> P.Lambda (tau, arg_names, case_exp body)
    | A.Thunk _ -> failwith " thunk impossible "
    (* | A.Case _ -> failwith " case impossible " *)
    | A.Tuple es -> P.Tuple (List.map case_exp es)
    | A.At (e, idx) -> P.At (case_exp e, idx)
    | A.Noexpr -> P.Noexpr
  in

  let rec case_def : A.def -> P.def = function
    | A.Exp e -> P.Exp (case_exp e)
    | A.Function (t, name, formals, body) ->
        P.Function (t, name, formals, case_exp body)
    | A.Datatype (t, cs) -> P.Datatype (t, cs)
    | A.Variable (t, n, e) -> P.Variable (t, n, case_exp e)
    | A.CheckTypeError d -> P.CheckTypeError (case_def d)
  in

  List.map case_def program
