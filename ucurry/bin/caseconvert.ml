module A = Ast 
module P = Past
module StringMap = Map.Make (String)

type vcon_env = (string * int * A.typ) StringMap.t

let add_vcon (vcon_env: vcon_env) (def: A.def) : vcon_env = 
    match def with 
        | A.Datatype (A.CONSTRUCTOR_TY (dt_name, _), cons) -> 
            let add (vcon, arg_tau) i env = 
                StringMap.add vcon (dt_name, i, arg_tau) env
            in 
            Util.fold_left_i add 1 vcon_env cons 
        | _ -> vcon_env

let case_convert (program: A.def list) : P.def list = 
    let vcon_env = List.fold_left add_vcon StringMap.empty program in 

    (* TODO: double check type casting *)
    let rec gen (scrutinee: A.expr) (ces: A.case_expr list) (resume: P.expr) : P.expr = 
    ( match ces with 
        | (hd, e)::rest ->
            (match hd with 
                | A.VAR_PAT n -> P.Let ([(n, case_exp scrutinee)], case_exp e)
                | A.WILDCARD -> case_exp e
                | A.CON_PAT (vcon, p) ->
                    let cond_exp = P.Binop (P.GetTag (case_exp scrutinee), A.Equal, P.Literal (A.STRING vcon)) in 
                    let _, vcon_id, _ = StringMap.find vcon vcon_env in 
                    let then_exp = gen (A.GetField (scrutinee, vcon_id)) [(p, e)] (gen scrutinee rest resume) in 
                    let else_exp = (gen scrutinee rest resume) in 
                    P.If (cond_exp, then_exp, else_exp)
                | A.PATS _ -> failwith "not yet implemente PATS in Case")
        | [] -> resume
        | _ -> failwith "impossible")
    
    
    and case_exp : A.expr -> P.expr = function
        | A.Case (scrutinee, ces) -> (match ces with 
            | (p, e)::rest -> gen scrutinee ces (case_exp e) 
            | _ -> failwith "at least one case in pattern matching should be provided")
        | _ -> failwith "impossible"
    in

    let rec case_def : A.def -> P.def = function
        | A.Exp e -> P.Exp (case_exp e)
        | A.Function (t, name, formals, body) -> 
            P.Function (t, name, formals, case_exp body) 
        | A.Datatype (t,cs) -> P.Datatype (t,cs)
        | A.Variable (t, n, e) -> P.Variable (t, n, case_exp e)
        | A.CheckTypeError d -> P.CheckTypeError (case_def d)
    in

    let past = List.map case_def program in 
    past 
