-- datatype
datatype List = Empty | IntList of int list | StringList of string list;
datatype Color = Red | Blue;
check_type_error List i = (IntList ["c", "d"]);
check_type_error List i = (Empty ["c", "d"]);
check_type_error Color i = (Empty);
check_type_error Unbound i = (Empty);
check_type_error List i = (Unbound);


-- case
datatype Background = Gradient of (int * Color) | Pure of Color; 
datatype ColorScale = Red of int |  Blue of int ;
datatype Nat = Zero | TenTimesPlus of (Nat * int);
datatype Shape = Circle | Square;
check_type_error (case (Circle) of Circle => 1 | Square => "square");
check_type_error (case (Circle) of Zero => 0);
check_type_error int n = (case (ZERO) of Red => 1);