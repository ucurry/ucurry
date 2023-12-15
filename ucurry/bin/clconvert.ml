(* Closure Conversion (SAST -> CAST) *)
(* Authors: Alex Bai, Stephanie Xu, Vivian Li, Matt Zhou *)

open Typing
module A = Ast
module SA = Sast
module C = Cast

type freevar = typ * string

module FreeVar : Set.OrderedType with type t = freevar = struct
  type t = freevar

  let compare (_, s1) (_, s2) = String.compare s1 s2
end

module S = Set.Make (FreeVar)

exception CLOSURE_NOT_YET_IMPLEMENTED of string

(* do free variable analysis *)
let rec free ((t, exp) : SA.sexpr) : S.t =
  let unionSets (sets : S.t list) : S.t =
    List.fold_left (fun acc s -> S.union s acc) S.empty sets
  in
  let unionFree (sexprs : SA.sexpr list) : S.t =
    unionSets (List.map free sexprs)
  in

  match exp with
  | SA.SLiteral _ -> S.empty
  | SA.SVar name -> S.of_list [ (t, name) ]
  | SA.SApply (f, thunks) -> unionFree (f :: thunks)
  | SA.SIf (s1, s2, s3) -> unionFree [ s1; s2; s3 ]
  | SA.SLet (bindings, body) ->
      let names, thunks = List.split bindings in
      let tau, _ = List.split thunks in
      let localsWithTypes = List.combine tau names in
      let freeXSet = S.of_list localsWithTypes in
      let freeESet = unionFree thunks in
      let freeBody = free body in
      S.union freeESet (S.diff freeBody freeXSet)
  | SA.SLetrec (bindings, body) ->
      let names, thunks = List.split bindings in
      let tau, _ = List.split thunks in
      let localsWithTypes = List.combine tau names in
      let freeXSet = S.of_list localsWithTypes in
      let freeESet = unionFree thunks in
      let freeBody = free body in
      S.diff (S.union freeESet freeBody) freeXSet
  | SA.SBegin sexprs -> unionFree sexprs
  | SA.SBinop (operand1, _, operand2) -> S.union (free operand1) (free operand2)
  | SA.SUnop (_, operand) -> free operand
  | SA.SConstruct (_, arg) -> free arg
  | SA.SLambda (formals, sexpr) ->
      let formalTypes = Util.get_formalty t in
      let formalWithTypes = List.combine formalTypes formals in
      S.diff (free sexpr) (S.of_list formalWithTypes)
  | SA.STuple ses -> unionFree ses
  | SA.SEmptyList _ -> S.empty
  | SA.SList (hd, tl) -> unionFree [ hd; tl ]
  | SA.SAt (se, _) -> free se
  | SA.SGetTag se -> free se
  | SA.SGetField (se, _) -> free se
  | SA.SNoexpr -> S.empty
  | SA.SNomatch -> S.empty
  | SA.SThunk se -> free se 
  | SA.SForce se -> free se

let indexOf (x : string) (xs : freevar list) : int option =
  let rec indexOf' (x : string) (xs : freevar list) (i : int) : int option =
    match xs with
    | [] -> None
    | (_, y) :: ys -> if x = y then Some i else indexOf' x ys (i + 1)
  in
  indexOf' x xs 0

let rec asClosure (funty : typ) (lambda : SA.lambda) (captured : freevar list) :
    C.closure =
  let formals, sbody = lambda in
  let freeVarWithTypes = S.elements (free (funty, SA.SLambda lambda)) in
  let captured' =
    List.map (fun (t, n) -> close_exp captured (t, SA.SVar n)) freeVarWithTypes
  in
  ((formals, close_exp freeVarWithTypes sbody), captured')

(* close expression *)
and close_exp (captured : freevar list) (le : SA.sexpr) : C.sexpr =
  let rec exp ((ty, tope) : SA.sexpr) : C.sexpr =
    ( ty,
      match tope with
      | SA.SLiteral l -> C.Literal l
      | SA.SVar name -> (
          match indexOf name captured with
          | Some i -> C.Captured i
          | None -> C.Var name (* this case, name is a local *))
      | SA.SApply (f, ls) -> C.Apply (exp f, List.map exp ls)
      | SA.SIf (i, t, e) -> C.If (exp i, exp t, exp e)
      | SA.SBegin es -> C.Begin (List.map exp es)
      | SA.SBinop (l, o, r) -> C.Binop (exp l, o, exp r)
      | SA.SUnop (op, se) -> C.Unop (op, exp se)
      | SA.SLet (ls, e) ->
          let ls' = List.map (fun (name, l) -> (name, exp l)) ls in
          (* need to recheck*)
          let e' = exp e in
          C.Let (ls', e')
      | SA.SLetrec (ls, e) ->
          let ls' = List.map (fun (name, l) -> (name, exp l)) ls in
          (* need to recheck*)
          let e' = exp e in
          C.Letrec (ls', e')
      | SA.SConstruct (i, arg) -> C.Construct (i, exp arg)
      | SA.SEmptyList tau -> C.EmptyList tau
      | SA.SList (hd, tl) -> C.List (exp hd, exp tl)
      | SA.SLambda lambda -> C.Closure (asClosure ty lambda captured)
      | SA.SNoexpr -> Noexpr
      | SA.STuple ses -> C.Tuple (List.map exp ses)
      | SA.SAt (se, i) -> C.At (exp se, i)
      | SA.SGetField (se, i) -> C.GetField (exp se, i)
      | SA.SGetTag se -> C.GetTag (exp se)
      | SA.SNomatch -> C.Nomatch 
      | SA.SThunk se -> C.Thunk (exp se)
      | SA.SForce se -> C.Force (exp se))
  in
  exp le

(* close definition *)
let close (def : SA.sdef) : Cast.def =
  match def with
  | SA.SExp e -> C.Exp (close_exp [] e)
  (* 106 uses C.Funcode, probably because toplevel function
     can only capture global veriables*)
  | SA.SFunction (t, name, lambda) -> 
      let closure = asClosure t lambda [] in
      C.Function (t, name, closure)
  | SA.SDatatype (t, cons) -> C.Datatype (t, cons)
  | SA.SCheckTypeError _ ->
      raise (CLOSURE_NOT_YET_IMPLEMENTED "check type error")
  | SA.SVal (name, se) -> C.Val (name, close_exp [] se)

let close_program (p : SA.sprogram) : C.program = List.map close p
