-- (lambda (f) (lambda (x) (lambda (y) (f x y))))

fun : int -> int -> int :
add2 x y = x + y;

int res = (\((int -> int -> int) -> int -> int -> int) f -> \(int -> int -> int) x -> \(int -> int) y -> (f x y) add2 1 2);
println res;
