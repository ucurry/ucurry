fun : (int -> int) -> int -> int :
apply f x = (f x);

fun : int -> int :
addone x = x + 1;

println (apply addone 3);
