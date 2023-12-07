module A = Ast 

let rec gen (scrutinee: A.expr) (ces: A.case_expr list) (resume: A.expr) : A.expr = 
 ( match ces with 
    | (hd, e)::rest ->
        (match hd with 
            | A.VAR_PAT n -> A.Let ([(n, scrutinee)], e)
            | A.WILDCARD -> e
            | A.CON_PAT (vcon, p) ->
                let cond_exp = A.Binop (A.GetTag scrutinee, A.Equal, A.Literal (A.STRING vcon)) in 
                let then_exp = gen (A.GetField scrutinee) [(p, e)] (gen scrutinee rest resume) in 
                let else_exp = (gen scrutinee rest resume) in 
                A.If (cond_exp, then_exp, else_exp)
            | A.PATS _ -> failwith "not yet implemente PATS in Case")
    | [] -> resume
    | _ -> failwith "impossible")