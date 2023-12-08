(* open Typing  *)
module A = Ast
module P = Past
module StringMap = Map.Make (String)

let case_convert (program : A.def list) : P.def list =
  (* TODO: double check type casting *)
  let rec gen (scrutinee : A.expr) (ces : A.case_expr list) (resume : P.expr) :
      P.expr =
    match ces with
    | (hd, e) :: rest -> (
        match hd with
        | A.VAR_PAT n -> P.Let ([ (n, case_exp scrutinee) ], case_exp e)
        | A.WILDCARD -> case_exp e
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
        | A.PATS [] -> case_exp e
        | A.PATS _ -> failwith "not yet implemente PATS in Case")
    | [] -> resume
  and case_exp : A.expr -> P.expr = function
    | A.Literal v -> P.Literal v
    | A.Construct (vcon_name, arg) -> P.Construct (vcon_name, case_exp arg)
    | A.Case (_, []) ->
        failwith "at least one case in pattern matching should be provided"
    | A.Case (scrutinee, ces) -> gen scrutinee ces P.Nomatch
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
    | A.Apply _ -> failwith " apply impossible "
    | A.Lambda _ -> failwith " lambda impossible "
    | A.Thunk _ -> failwith " thunk impossible "
    (* | A.Case _ -> failwith " case impossible " *)
    | A.Tuple es -> P.Tuple (List.map case_exp es)
    | A.At _ -> failwith "at impossible"
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
