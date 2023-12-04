fun: int -> int:
f x = x + 1;

fun: int -> int:
g x = x + 1;

fun: (int -> int) -> (int -> int) -> int -> int:
o f g x = (f (g x));

println (o f g 1);

