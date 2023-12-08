type name = string
type dt_name = string
type vcon_name = string
type vcon_id = int
type arg_name = string

type typ =
  | INT_TY
  | STRING_TY
  | BOOL_TY
  | LIST_TY of typ
  | UNIT_TY
  | FUNCTION_TY of typ * typ
  | CONSTRUCTOR_TY of dt_name
  | TUPLE_TY of typ list
  | ANY_TY


let rec string_of_typ = function
  | INT_TY -> "int"
  | STRING_TY -> "string"
  | BOOL_TY -> "bool"
  | LIST_TY typ -> string_of_typ typ ^ " list"
  | UNIT_TY -> "unit"
  | FUNCTION_TY (t1, t2) ->
      "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
  | CONSTRUCTOR_TY dt -> dt
  | TUPLE_TY typs ->
      "(" ^ String.concat " * " (List.map string_of_typ typs) ^ ")"
  | ANY_TY -> "anytyp"