fun: int -> int -> int:
f x y = x + y;
println (f 1 2);

fun: bool -> int -> int -> int:
g b i n = if b then i else n;

println (g true 1 2);