fun : (int -> int) -> int :
add2 x = 2;

int res = (\(((int -> int) -> int) -> int) f -> 2 add2);
println res;