(* module A = Ast *)
module A = Ast
module SA = Sast
module L = Last
module C = Cast

type freevar = A.typ * string

module FreeVar : Set.OrderedType with type t = freevar = struct
  type t = freevar

  let compare (_, s1) (_, s2) = String.compare s1 s2
end

module S = Set.Make (FreeVar)

exception CLOSURE_NOT_YET_IMPLEMENTED of string

(* do free variable analysis *)
let rec free ((t, exp) : L.sexpr) : S.t =
  let unionSets (sets : S.t list) : S.t =
    List.fold_left (fun acc s -> S.union s acc) S.empty sets
  in
  let unionFree (sexprs : L.sexpr list) : S.t =
    unionSets (List.map free sexprs)
  in

  match exp with
  | L.Literal _ -> S.empty
  | L.Var name -> S.of_list [ (t, name) ]
  | L.Assign (assigned, thunk) ->
      S.union (free thunk) (S.of_list [ (t, assigned) ])
  | L.Apply (f, thunks) -> unionFree (f :: thunks)
  | L.If (s1, s2, s3) -> unionFree [ s1; s2; s3 ]
  | L.Let (bindings, body) ->
      let localsWithTypes, thunks = List.split bindings in
      let freeXSet = S.of_list localsWithTypes in
      let freeESet = unionFree thunks in
      let freeBody = free body in
      S.union freeESet (S.diff freeBody freeXSet)
  | L.Begin sexprs -> unionFree sexprs
  | L.Unop (_, operand) -> free operand
  | L.Binop (operand1, _, operand2) -> S.union (free operand1) (free operand2)
  | L.Case (scrutinee, cexprs) ->
      let fscrutinee = free scrutinee in
      List.fold_left
        (fun freevars (_, sexpr) -> S.union freevars (free sexpr))
        fscrutinee cexprs
  | L.Lambda (formals, sexpr) ->
      let formalTypes = Util.get_formalty t in
      (* let _ = print_int (List.length formalTypes) in *)
      (* let _ = print_int (List.length formals) in       *)
      let formalWithTypes = List.combine formalTypes formals in
      S.diff (free sexpr) (S.of_list formalWithTypes)
  | L.At (sexpr, _) -> free sexpr (*TODO dc this w/ Vivian*)
  | L.Noexpr -> S.empty

let indexOf (x : string) (xs : freevar list) : int option =
  let rec indexOf' (x : string) (xs : freevar list) (i : int) : int option =
    match xs with
    | [] -> None
    | (_, y) :: ys -> if x = y then Some i else indexOf' x ys (i + 1)
  in
  indexOf' x xs 0

let rec asClosure (lambda : L.sexpr) (captured : freevar list) : C.closure =
  match lambda with
  | _, L.Lambda (formals, body) ->
      let freeVarWithTypes = S.elements (free lambda) in
      let captured' =
        List.map
          (fun (t, n) -> closeExpWith captured (t, L.Var n))
          freeVarWithTypes
      in
      ((formals, closeExpWith freeVarWithTypes body), captured')
  | _ -> failwith "Non-lambda encountered in asClosure"

(* close expression *)
and closeExpWith (captured : freevar list) (le : L.sexpr) : C.sexpr =
  let rec exp ((ty, tope) : L.sexpr) : C.sexpr =
    ( ty,
      match tope with
      | L.Literal l -> C.Literal l
      | L.Var name -> (
          match indexOf name captured with
          | Some i -> C.Captured i
          | None -> C.Var name (* this case, name is a local *))
      | L.Assign (name, e) -> (
          (* cannot captured local cannot be set, why? *)
          match indexOf name captured with
          (* | Some _ -> C.Assign (name, exp e) *)
          | Some _ -> raise (Failure "Cannot assign to captured variable")
          | None -> C.Assign (name, exp e))
      | L.Apply (f, ls) -> C.Apply (exp f, List.map exp ls)
      | L.If (i, t, e) -> C.If (exp i, exp t, exp e)
      | L.Begin es -> C.Begin (List.map exp es)
      | L.Binop (l, o, r) -> C.Binop (exp l, o, exp r)
      | L.Unop (op, se) -> C.Unop (op, exp se)
      | L.Let (ls, e) ->
          let ls' = List.map (fun (name, l) -> (name, exp l)) ls in
          (* need to recheck*)
          let e' = exp e in
          C.Let (ls', e')
      | L.Case (scrutinee, cases) ->
          let scrutinee' = exp scrutinee in
          let cases' = List.map (fun (p, e) -> (p, exp e)) cases in
          C.Case (scrutinee', cases')
      | L.Lambda lambda -> C.Closure (asClosure (ty, L.Lambda lambda) captured)
      | L.Noexpr -> Noexpr
      | L.At (se, i) -> C.At (exp se, i) )
  in
  exp le

(* close definition *)
let close (def : L.def) : Cast.def =
  match def with
  (* 106 uses C.Funcode, probably because toplevel function
     can only capture global veriables*)
  | L.Function (t, name, lambda) ->
      let closure = asClosure lambda [] in
      C.Function (t, name, closure)
  | L.Datatype (t, cons) -> C.Datatype (t, cons)
  | L.Exp e -> C.Exp (closeExpWith [] e)
  | L.CheckTypeError _ -> raise (CLOSURE_NOT_YET_IMPLEMENTED "check type error")
(* | L.Val (ty, name, se) -> C.Val (ty, name, closeExpWith [] se) *)

let closeProgram (p : L.program) : C.program = List.map close p
