int y = ~5;

fun: int -> int:
loop z = if z > 0 then z else (loop z);

fun: int -> int:
f x = if y > 8 then x else ~y;

int test = (f (loop y));
println test;

