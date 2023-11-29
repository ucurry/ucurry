-- this `apply` is a dummy higher-order function that 
-- takes in a function and its argument and applies it
fun : (int -> int) -> int -> int :
apply f x = (f x);

fun : int -> int :
addone x = x + 1;

println (apply addone 3);
