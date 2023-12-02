-- lazy evaluation prevents infinite recursion
-- see notinf3.uc for a detailed description

fun: int -> (int -> int) -> int:
fst x y = x;

fun: int -> int:
f a = (f a);

println (fst 1 f);
