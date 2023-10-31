fun: int -> int:
f x = x + 1;
check_type_error (f 1 2); 
check_type_error (f true); 

fun: int -> int -> int:
add x y = x + y;
check_type_error ((add 1) 2 2);
check_type_error int -> int->int add3 = (add 3);

