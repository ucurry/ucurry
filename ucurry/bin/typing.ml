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
