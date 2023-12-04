
fun : int -> int -> int :
add2 x y = x + y;

int res = (\((int -> int -> int) -> int) f -> 2 add2);
println res;